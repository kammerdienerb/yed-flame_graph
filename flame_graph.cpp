#include <memory>
#include <vector>
#include <map>
#include <unordered_set>
#include <string>
#include <string_view>
#include <cstring>
#include <fstream>
#include <functional>

extern "C" {
#include <yed/plugin.h>
}

template<typename F>
class defer_finalizer {
    F f;
    bool moved;
  public:
    template<typename T>
    defer_finalizer(T && f_) : f(std::forward<T>(f_)), moved(false) { }

    defer_finalizer(const defer_finalizer &) = delete;

    defer_finalizer(defer_finalizer && other) : f(std::move(other.f)), moved(other.moved) {
        other.moved = true;
    }

    ~defer_finalizer() {
        if (!moved) f();
    }
};

struct {
    template<typename F>
    defer_finalizer<F> operator<<(F && f) {
        return defer_finalizer<F>(std::forward<F>(f));
    }
} deferrer;

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define defer auto TOKENPASTE2(__deferred_lambda_call, __COUNTER__) = deferrer << [&]


#define DBG_LOG_ON

#define LOG__XSTR(x) #x
#define LOG_XSTR(x) LOG__XSTR(x)

#define LOG(...)                                                   \
do {                                                               \
    LOG_FN_ENTER();                                                \
    yed_log(__VA_ARGS__);                                          \
    LOG_EXIT();                                                    \
} while (0)

#define ELOG(...)                                                  \
do {                                                               \
    LOG_FN_ENTER();                                                \
    yed_log("[!] " __VA_ARGS__);                                   \
    LOG_EXIT();                                                    \
} while (0)

#ifdef DBG_LOG_ON
#define DBG(...)                                                   \
do {                                                               \
    if (yed_var_is_truthy("flame-graph-debug-log")) {              \
        LOG_FN_ENTER();                                            \
        yed_log(__FILE__ ":" LOG_XSTR(__LINE__) ": " __VA_ARGS__); \
        LOG_EXIT();                                                \
    }                                                              \
} while (0)
#else
#define DBG(...) ;
#endif


#define BUFF_WRITABLE_GUARD(_buff)             \
    (_buff)->flags &= ~(BUFF_RD_ONLY);         \
    defer { (_buff)->flags |= BUFF_RD_ONLY; };




static void unload(yed_plugin *self) {}

struct String_Intern_Table {
    template<typename ... Bases>
    struct Overload : Bases ... {
        using is_transparent = void;
        using Bases::operator() ... ;
    };


    struct Char_Pointer_Hash {
        auto operator()(const char *ptr) const noexcept {
            return std::hash<std::string_view>{}(ptr);
        }
    };

    using Transparent_String_Hash = Overload<
        std::hash<std::string>,
        std::hash<std::string_view>,
        Char_Pointer_Hash
    >;


    std::unordered_set<std::string, Transparent_String_Hash, std::equal_to<>> tab;

    const char *intern(const std::string_view &sv) {
        auto lookup = this->tab.find(sv);
        if (lookup != this->tab.end()) {
            return lookup->data();
        }
        return this->tab.insert(std::string(sv)).first->data();
    }
};

static String_Intern_Table stab;

struct String_Compare {
    bool operator() (const char* str1, const char* str2) const { return std::strcmp(str1, str2) < 0; }
};

struct Flame_Graph {
        enum Frame_Type {
            UNKNOWN,
            DIVIDER,
            GPU_INST,
            GPU_SYMBOL,
            KERNEL,
            C,
            CPP,
            PYTHON,
        };

        struct Frame {
            const char                       *label;
            size_t                            count = 0;
            std::map<const char*,
                     std::shared_ptr<Frame>,
                     String_Compare>          children;

            std::shared_ptr<Frame> add_frame(const char *label, size_t count) {
                auto f = this->children[label];
                if (!f) {
                    f = this->children[label] = std::make_shared<Frame>();
                    f->label = label;
                }
                f->count += count;
                return f;
            }
        };

        struct Frame_Info {
            u64                     row;
            u64                     start_col;
            u64                     end_col;
            Frame_Type              type;
            u64                     rand;
            std::shared_ptr<Frame>  frame;
        };


        std::shared_ptr<Frame>                    base;
        std::shared_ptr<Frame>                    true_base;
        std::string                               name;
        yed_buffer                               *buffer = NULL;
        size_t                                    max_depth = 0;
        std::map<u64, std::map<u64, Frame_Info>>  frame_info;

        Flame_Graph() : true_base(std::make_shared<Frame>()) {
            this->base        = this->true_base;
            this->base->label = stab.intern("all");
        }

        Flame_Graph(const Flame_Graph&) = delete;

        void add_flame(std::string &&s) {
            if (s.size() == 0) { return; }

            std::string count_string;
            while (is_digit(s.back())) {
                count_string.insert(count_string.begin(), s.back());
                s.pop_back();
            }
            while (is_space(s.back())) { s.pop_back(); }

            if (count_string.empty()) { return; }

            size_t count = std::stoll(count_string);

            const char *label;
            auto f = this->base;

            f->count += count;

            size_t last  = 0;
            size_t next  = 0;
            size_t depth = 0;
            std::string_view sv(s);
            while ((next = sv.find(';', last)) != std::string::npos) {
                label = stab.intern(sv.substr(last, next - last));

                f = f->add_frame(label, count);

                last = next + 1;
                depth += 1;
            }
            label = stab.intern(s.substr(last));
            depth += 1;

            if (depth > this->max_depth) { this->max_depth = depth; }
        }

        void write_buffer(size_t width) {
            DBG("write_buffer(this=%p)", this);

            if (this->buffer == NULL) {
                this->buffer = yed_get_or_create_special_rdonly_buffer((char*)this->name.c_str());
            }

            this->frame_info.clear();

            BUFF_WRITABLE_GUARD(this->buffer);

            yed_buff_clear_no_undo(this->buffer);
            for (size_t i = 1; i < this->max_depth; i += 1) { yed_buffer_add_line_no_undo(this->buffer); }

            std::function<void(std::shared_ptr<Frame>, size_t, size_t, size_t, int)>
            write_frame = [&write_frame,this](std::shared_ptr<Frame> f, size_t start_col, size_t width, size_t depth, int first_child) {

                /* Allow a one column flame to show that there's something there even if we can't read it. */
                if (width < 1) { return; }

                Frame_Type type = Frame_Type::UNKNOWN;

                std::string s = f->label;

                if (s.ends_with("_[g]")) {
                    type = Frame_Type::GPU_INST;
                    s.erase(s.size() - 4);
                } else if (s.ends_with("_[G]")) {
                    type = Frame_Type::GPU_SYMBOL;
                    s.erase(s.size() - 4);
                } else if (s.ends_with("_[k]")) {
                    type = Frame_Type::KERNEL;
                    s.erase(s.size() - 4);
                } else if (s.starts_with("py::")) {
                    type = Frame_Type::PYTHON;
                } else if (s.find("::") != std::string::npos) {
                    type = Frame_Type::CPP;
                } else if (s == "-") {
                    type = Frame_Type::DIVIDER;
                }

//                 if (first_child) {
//                     s.insert(s.begin(), '|');
//                 }

//                 if (s.size() > width - 1 && width > !!first_child + 3) {
//                     s[width - 3] = '.';
//                     s[width - 2] = '.';
//                 }

//                 s = s.substr(0, width - 1);

                if (width == 1) {
                    s = "";
                } else {
                    if (s.size() > width - 1 && width > 2) {
                        s[width - 3] = '.';
                        s[width - 2] = '.';
                    }

                    s = s.substr(0, width - 1);
                }

                yed_buff_insert_string_no_undo(this->buffer, s.c_str(), depth, start_col);

                size_t child_off = 0;
                for (auto pair : f->children) {
                    auto child = pair.second;
                    size_t child_width = ((float)child->count / (float)f->count) * width;

                    write_frame(child, start_col + child_off, child_width, depth - 1, child_off == 0);
                    child_off += child_width;
                }
                yed_buff_insert_string_no_undo(this->buffer, " ", depth, start_col + width - 1);

                Frame_Info info;
                info.row       = depth;
                info.start_col = start_col;
                info.end_col   = start_col + width - 1;
                info.type      = type;
                info.rand      = rand();
                info.frame     = f;

                this->frame_info[depth][start_col] = info;
            };

            write_frame(this->base, 1, width, this->max_depth, 1);
        }
};

static std::map<std::string, Flame_Graph> flame_graphs;


static yed_plugin *Self;

static void flame_graph(int n_args, char **args) {
    if (n_args != 1) {
        yed_cerr("expected 1 argument, but got %d", n_args);
        return;
    }

    std::ifstream f(args[0]);

    if (!f) {
        yed_cerr("failed to open flame graph file '%s'", args[0]);
        return;
    }

    flame_graphs.erase(args[0]);

    auto &graph = flame_graphs[args[0]];
    graph.name = std::string("*flame-graph:") + args[0] + "";

    std::string line;
    while (std::getline(f, line)) {
        graph.add_flame(std::move(line));
    }

    graph.write_buffer(ys->active_frame == NULL ? 120 : ys->active_frame->width);

    YEXE("buffer", (char*)graph.name.c_str());
    YEXE("cursor-buffer-end");
    YEXE("cursor-line-begin");
}

static void fit(yed_event *event) {
    for (auto &pair : flame_graphs) {
        auto       &g = pair.second;
        int         in_frame;
        int         width;
        yed_frame **fit;
        yed_frame  *f;

        in_frame = 0;
        width    = INT_MAX;

        array_traverse(ys->frames, fit) {
            f = *fit;
            if (f->buffer != g.buffer) { continue; }

            in_frame = 1;

            if (f->width < width) {
                width = f->width;
            }
        }

        if (in_frame) {
            g.write_buffer(width);
        }
    }
}

static void hsv_to_rgb(float h, float s, float v,
                       int  *r, int  *g, int  *b) {

    float R;
    float G;
    float B;
    float C;
    float X;
    float m;

    C = v * s;
    X = C * (1 - fabs(fmod(h/(M_PI/3.0), 2.0) - 1.0));
    m = v - C;

    if      (h >= 0.0              && h < (M_PI/3.0))       { R = C; G = X; B = 0; }
    else if (h >= (M_PI/3.0)       && h < (2.0 * M_PI/3.0)) { R = X; G = C; B = 0; }
    else if (h >= (2.0 * M_PI/3.0) && h < (M_PI))           { R = 0; G = C; B = X; }
    else if (h >= (M_PI/2.0)       && h < (4.0 * M_PI/3.0)) { R = 0; G = X; B = C; }
    else if (h >= (4.0 * M_PI/3.0) && h < (5.0 * M_PI/3.0)) { R = X; G = 0; B = C; }
    else if (h >= (5.0 * M_PI/3.0) && h < (2.0*M_PI))       { R = C; G = 0; B = X; }
    else                                                    { R =    G =    B = 0; }

    *r = (R + m) * 255;
    *g = (G + m) * 255;
    *b = (B + m) * 255;
}

static void color(yed_event *event) {
    yed_frame  *frame;
    yed_buffer *buff;

    frame = event->frame;
    if (frame == NULL) { return; }

    buff = frame->buffer;
    if (buff == NULL) { return; }

    for (auto &pair : flame_graphs) {
        auto &g = pair.second;

        if (g.buffer != buff) { continue; }

        for (auto &pair : g.frame_info[event->row]) {
            auto &info = pair.second;

            yed_attrs attrs = ZERO_ATTR;

            if (ys->current_search != NULL
            &&  ys->current_search[0]
            &&  strstr(info.frame->label, ys->current_search) != NULL) {

                attrs = yed_parse_attrs("&black bg ff00ff");
            } else {
                using Frame_Type = Flame_Graph::Frame_Type;

                float h, s = 0.5, v = 0.75;

                switch (info.type) {
                    case Frame_Type::GPU_INST:
                        h = 0.6 * M_PI;
                        break;
                    case Frame_Type::GPU_SYMBOL:
                        h = M_PI;
                        break;
                    case Frame_Type::KERNEL:
                        h = 0.15 * M_PI;
                        break;
                    case Frame_Type::CPP:
                        h = 0.25 * M_PI;
                        break;
                    case Frame_Type::PYTHON:
                        h = 0.0;
                        s = 0.25;
                        break;
                    case Frame_Type::DIVIDER:
                        h = 0.0;
                        s = 0.0;
                        v = 0.5;
                        break;
                    default:
                        h = 0.0;
                        break;
                }

                if (info.type != Frame_Type::DIVIDER) {
                    v += ((float)((info.rand % 1000) + 1) / 1000.0) * 0.15;
                }

                int r, g, b;
                hsv_to_rgb(h, s, v, &r, &g, &b);

                char attr_string[512];
                snprintf(attr_string, sizeof(attr_string), "&black bg %02x%02x%02x", r, g, b);
                DBG("%s", attr_string);
                attrs = yed_parse_attrs(attr_string);
            }

            for (int col = info.start_col; col <= info.end_col; col += 1) {
                yed_eline_combine_col_attrs(event, col, &attrs);
            }
        }

        break;
    }
}

static void key(yed_event *event) {
    yed_frame  *frame;
    yed_buffer *buff;

    if (ys->interactive_command
    ||  !ys->active_frame) {
        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    buff = frame->buffer;
    if (buff == NULL) { return; }

    if (event->key != ENTER) { return; }

    int row = frame->cursor_line;
    int col = frame->cursor_col;

    for (auto &pair : flame_graphs) {
        auto &g = pair.second;

        if (g.buffer != buff) { continue; }

        for (auto &pair : g.frame_info[row]) {
            auto &info = pair.second;

            if (info.start_col <= col || col <= info.end_col) {
                g.base = info.frame;
                g.write_buffer(frame->width);
                break;
            }
        }

        break;
    }

    return;

found:;
}

extern "C"
int yed_plugin_boot(yed_plugin *self) {
    char *state_addr_str;
    char  addr_buff[64];

    YED_PLUG_VERSION_CHECK();

    Self = self;

    std::map<void(*)(yed_event*), std::vector<yed_event_kind_t> > event_handlers = {
        { fit,   { EVENT_FRAME_POST_RESIZE, EVENT_TERMINAL_RESIZED,
                   EVENT_FRAME_POST_DELETE, EVENT_FRAME_PRE_SET_BUFFER,
                   EVENT_FRAME_POST_SET_BUFFER } },
        { color, { EVENT_LINE_PRE_DRAW } },
        { key,   { EVENT_KEY_PRESSED   } }};

    std::map<const char*, const char*> vars = {
        { "flame-graph-debug-log", "ON" }};

    std::map<const char*, void(*)(int, char**)> cmds = {
        { "flame-graph", flame_graph }};

    for (auto &pair : event_handlers) {
        for (auto evt : pair.second) {
            yed_event_handler h;
            h.kind = evt;
            h.fn   = pair.first;
            yed_plugin_add_event_handler(self, h);
        }
    }

    for (auto &pair : vars) {
        if (!yed_get_var(pair.first)) { yed_set_var(pair.first, pair.second); }
    }

    for (auto &pair : cmds) {
        yed_plugin_set_command(self, pair.first, pair.second);
    }
    yed_plugin_set_completion(self, "flame-graph-compl-arg-0", yed_get_completion("file"));

    yed_plugin_set_unload_fn(self, unload);

    return 0;
}

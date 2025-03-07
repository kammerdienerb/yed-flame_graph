#include <memory>
#include <vector>
#include <map>
#include <unordered_set>
#include <string>
#include <string_view>
#include <cstring>
#include <fstream>
#include <sstream>
#include <functional>
#include <algorithm>

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
            const char                       *label            = NULL;
            const char                       *cleaned_up_label = NULL;
            size_t                            count            = 0;
            std::map<const char*,
                     std::shared_ptr<Frame>,
                     String_Compare>          children;
            Frame                            *parent;

            Frame *add_frame(const char *label, size_t count) {
                auto f = this->children[label];
                if (!f) {
                    f = this->children[label] = std::make_shared<Frame>();
                    f->parent = this;
                    f->label  = label;
                }
                f->count += count;
                return f.get();
            }
        };

        struct Frame_Info {
            u64         row;
            u64         start_col;
            u64         end_col;
            Frame_Type  type;
            u64         rand;
            Frame      *frame;
        };


        std::shared_ptr<Frame>                  true_base;
        std::vector<Frame*>                     base_stack;
        std::string                             name;
        yed_buffer                             *buffer = NULL;
        size_t                                  max_depth = 0;
        std::map<u64, std::vector<Frame_Info>>  frame_info;

        Flame_Graph() : true_base(std::make_shared<Frame>()) {
            this->base_stack.push_back(this->true_base.get());
            this->true_base->label = stab.intern("all");
        }

        Flame_Graph(const Flame_Graph&) = delete;

        Frame *get_base() { return this->base_stack.back(); }

        void add_flame(std::string &&s) {
            if (s.size() == 0) { return; }

            std::string count_string;
            while (is_digit(s.back())) {
                count_string.insert(count_string.begin(), s.back());
                s.pop_back();
            }
            while (is_space(s.back())) { s.pop_back(); }

            if (count_string.empty()) { return; }

            size_t count = std::stoull(count_string);

            const char *label;
            auto f = this->get_base();

            f->count += count;

            size_t last  = 0;
            size_t next  = 0;
            size_t depth = 1; /* Implicit 'all' frame. */
            std::string_view sv(s);
            while ((next = sv.find(';', last)) != std::string::npos) {
                label = stab.intern(sv.substr(last, next - last));

                f = f->add_frame(label, count);

                last = next + 1;
                depth += 1;
            }
            if (last < sv.size()) {
                label = stab.intern(sv.substr(last));
                f = f->add_frame(label, count);
                depth += 1;
            }

            if (depth > this->max_depth) { this->max_depth = depth; }
        }

        void write_buffer(size_t width) {
            if (this->buffer == NULL) {
                this->buffer = yed_get_or_create_special_rdonly_buffer((char*)this->name.c_str());
            }

            this->frame_info.clear();

            BUFF_WRITABLE_GUARD(this->buffer);

            yed_buff_clear_no_undo(this->buffer);
            for (size_t i = 1; i < this->max_depth; i += 1) { yed_buffer_add_line_no_undo(this->buffer); }

            std::function<void(Frame*, size_t, size_t, size_t)>
            write_frame = [&write_frame,this](Frame *f, size_t start_col, size_t width, size_t depth) {

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

                f->cleaned_up_label = stab.intern(s);

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
                std::vector<Frame*> sorted_children;

                for (auto pair : f->children) {
                    auto pos = std::lower_bound(
                        sorted_children.begin(),
                        sorted_children.end(),
                        pair.second.get(),
                        [](Frame *a, Frame *b) -> bool { return a->count > b->count; });
                    sorted_children.insert(pos, pair.second.get());
                }


                for (auto child : sorted_children) {
                    size_t child_width = ((float)child->count / (float)f->count) * width;
                    if (child_width < 1) { child_width = 1; }

                    if (child_off + child_width >= width) {
                        child_width = width - child_off;
                        if (child_width == 0) {
                            break;
                        }
                    }

                    write_frame(child, start_col + child_off, child_width, depth - 1);
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

                this->frame_info[depth].push_back(info);
            };

            write_frame(this->get_base(), 1, width, this->max_depth);
        }

        const Frame_Info *get_info(int row, int col) {
            for (auto &info : this->frame_info[row]) {
                if (info.start_col <= col && col <= info.end_col) {
                    return &info;
                }
            }
            return NULL;
        }

        int zoom(int row, int col, size_t width) {
            const Frame_Info *info = get_info(row, col);

            if (info == NULL) { return 0; }

            this->base_stack.push_back(info->frame);
            this->write_buffer(width);

            return 1;
        }

        int reset_zoom(size_t width) {
            if (this->get_base() == this->true_base.get()) { return 0; }

            this->base_stack.clear();
            this->base_stack.push_back(this->true_base.get());
            this->write_buffer(width);

            return 1;
        }

        int return_zoom(size_t width) {
            if (this->get_base() == this->true_base.get()) { return 0; }

            this->base_stack.pop_back();
            this->write_buffer(width);

            return 1;
        }
};

struct Popup {
    struct Popup_Text {
        std::string text;
    };

    std::vector<Popup_Text>  texts;
    std::vector<std::string> lines;
    array_t                  line_attrs;
    int                      row;
    int                      col;
    int                      max_width;

    Popup(int row, int col) : row(row), col(col), max_width(0) {
        this->line_attrs = array_make(array_t);
    }

    ~Popup() {
        array_t *it;

        array_traverse(this->line_attrs, it) {
            array_free(*it);
        }

        array_free(this->line_attrs);
    }

    void add_text(std::string &&text) {
        this->texts.emplace_back(Popup_Text{ std::move(text) });
    }

    void finish() {
        for (const auto &t : this->texts) {
            const auto &text  = t.text;

            std::istringstream is(text);

            std::string line;
            for (int l = 0; getline(is, line); l += 1) {
                int width = yed_get_string_width(line.c_str());
                if (width > this->max_width) { this->max_width = width; }

                array_t col_attrs = array_make(yed_attrs);
                for (int i = 0; i < width; i += 1) {
                    yed_attrs a = ZERO_ATTR;
                    array_push(col_attrs, a);
                }
                array_push(this->line_attrs, col_attrs);

                this->lines.push_back(line);
            }
        }
    }
};

static std::unique_ptr<Popup> popup;

static std::map<std::string, Flame_Graph> flame_graphs;

static std::unordered_set<Flame_Graph::Frame*> search_match_frames;


static yed_plugin *Self;

static Flame_Graph *graph_from_buffer(yed_buffer *buff) {
    if (buff == NULL) { return NULL; }

    for (auto &pair : flame_graphs) {
        auto &g = pair.second;
        if (g.buffer == buff) {
            return &g;
        }
    }

    return NULL;
}

static Flame_Graph *graph_in_frame(yed_frame *f) {
    return graph_from_buffer(f->buffer);
}


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


    u64 start = measure_time_now_ms();

    std::string line;
    while (std::getline(f, line)) {
        graph.add_flame(std::move(line));
    }

    popup.reset();

    graph.write_buffer(ys->active_frame == NULL ? 120 : ys->active_frame->width);

    DBG("took %llu ms to build %s", measure_time_now_ms() - start, graph.name.c_str());

    YEXE("buffer", (char*)graph.name.c_str());
    YEXE("cursor-buffer-end");
    YEXE("cursor-line-begin");
}

static void flame_graph_zoom(int n_args, char **args) {
    yed_frame *frame;

    if (n_args != 0) {
        yed_cerr("expected 0 argument, but got %d", n_args);
        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) {
        yed_cerr("no flame graph!");
        return;
    }

    popup.reset();

    if (graph->zoom(frame->cursor_line, frame->cursor_col, frame->width)) {
        YEXE("cursor-buffer-end");
        YEXE("cursor-line-begin");
    }
}

static void flame_graph_reset_zoom(int n_args, char **args) {
    yed_frame *frame;

    if (n_args != 0) {
        yed_cerr("expected 0 argument, but got %d", n_args);
        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) {
        yed_cerr("no flame graph!");
        return;
    }

    popup.reset();

    if (graph->reset_zoom(frame->width)) {
        YEXE("cursor-buffer-end");
        YEXE("cursor-line-begin");
    }
}

static void flame_graph_return_zoom(int n_args, char **args) {
    yed_frame *frame;

    if (n_args != 0) {
        yed_cerr("expected 0 argument, but got %d", n_args);
        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) {
        yed_cerr("no flame graph!");
        return;
    }

    popup.reset();

    if (graph->return_zoom(frame->width)) {
        YEXE("cursor-buffer-end");
        YEXE("cursor-line-begin");
    }
}

static void info_popup(Flame_Graph *graph, int row, int col) {
    const Flame_Graph::Frame_Info *info = graph->get_info(row, col);

    if (info == NULL) { return; }

    popup = std::make_unique<Popup>(row, col);

    popup->add_text(info->frame->cleaned_up_label);

    char buff[512];
    if (graph->get_base() == graph->true_base.get()) {
        snprintf(buff, sizeof(buff), "%zu samples (%.2f%% of all)", info->frame->count, 100.0 * (float)info->frame->count / (float)graph->get_base()->count);
    } else {
        snprintf(buff, sizeof(buff), "%zu samples (%.2f%% of total shown, %.2f%% of all)", info->frame->count, 100.0 * (float)info->frame->count / (float)graph->get_base()->count, 100.0 * (float)info->frame->count / (float)graph->true_base->count);
    }
    popup->add_text(buff);

    popup->finish();
}

static void flame_graph_frame_info(int n_args, char **args) {
    yed_frame *frame;

    if (n_args != 0) {
        yed_cerr("expected 0 argument, but got %d", n_args);
        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) {
        yed_cerr("no flame graph!");
        return;
    }

    info_popup(graph, frame->cursor_line, frame->cursor_col);
}

static void draw(yed_event *event) {
    if (!popup || popup->lines.size() == 0) { return; }

    if (ys->active_frame == NULL || ys->active_frame->buffer == NULL) { return; }

    yed_frame *f = ys->active_frame;
    int        y = yed_frame_line_to_y(f, popup->row);
    if (y <= 0) { return; }

    yed_line *line = yed_buff_get_line(f->buffer, popup->row);
    if (line == NULL) { return; }

    int col = popup->col;

    int end = col + popup->max_width + 2 - 1;
    if (end > f->width) {
        col -= end - f->width;
        if (col <= 0) { col = 1; }
    }

    if (col <= f->buffer_x_offset || col > f->buffer_x_offset + f->width) { return; }

    int x = f->left + col - (f->buffer_x_offset + 1);

    yed_attrs attrs = ZERO_ATTR;
    if (yed_active_style_get_popup().flags) {
        attrs = yed_parse_attrs("&active &popup");
    } else {
        attrs = yed_parse_attrs("&active &associate");
    }

    int n_lines     = popup->lines.size();
    int extra_lines = 2;
    int width       = popup->max_width;

    if (width + 2 > f->width) {
        width = f->width - 2;
    }
    width = MAX(width, 1);

    if (y > n_lines + extra_lines) {
        y -= n_lines + extra_lines;
    } else {
        y += 1;
    }

    yed_set_attr(attrs);

    for (int i = 0; i < n_lines + extra_lines; i += 1) {
        yed_set_cursor(y + i, x);
        if (i == 0) {
            yed_screen_print_over("┌");
        } else if (i == n_lines + extra_lines - 1) {
            yed_screen_print_over("└");
        } else {
            yed_screen_print_over("│");
        }
        for (int j = 1; j < width + 1; j += 1) {
            yed_set_cursor(y + i, x + j);
            if (i == 0 || i == n_lines + extra_lines - 1) {
                yed_screen_print_over("─");
            }
        }
        yed_set_cursor(y + i, x + width + 1);
        if (i == 0) {
            yed_screen_print_over("┐");
        } else if (i == n_lines + extra_lines - 1) {
            yed_screen_print_over("┘");
        } else {
            yed_screen_print_over("│");
        }
    }

    y += 1;

    int l = 0;
    for (const auto & line : popup->lines) {
        yed_glyph *git;
        int        c = 1;
        int        too_long = yed_get_string_width(line.c_str()) > width;
        array_t   *line_attrs = (array_t*)array_item(popup->line_attrs, l);

        yed_glyph_traverse(line.c_str(), git) {
            if (c > width) { break; }

            yed_set_cursor(y, x + c);

            yed_attrs a = attrs;

            if (too_long && c == width - 2) {
                yed_set_attr(attrs);
                yed_screen_print_n_over("...", 3);
                c += 3;
                break;
            }

            yed_combine_attrs(&a, (yed_attrs*)array_item(*line_attrs, c - 1));
            yed_set_attr(a);
            yed_screen_print_n_over(&git->c, yed_get_glyph_len(git));

            c += yed_get_glyph_width(git);
        }

        for (; c <= width; c += 1) {
            yed_set_cursor(y, x + c);
            yed_set_attr(attrs);
            yed_screen_print_n_over(" ", 1);
        }

        y += 1;
        l += 1;
    }
}

static void move(yed_event *event) {
    popup.reset();
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
    popup.reset();
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

static int search(Flame_Graph::Frame *f) {
    int match = 0;

    if (strstr(f->label, ys->current_search) != NULL) {
        match = 1;
    }

    for (auto pair : f->children) {
        match |= !!search(pair.second.get());
    }

    if (match) {
        search_match_frames.insert(f);
    }

    return match;
}

static void update(yed_event *event) {
    search_match_frames.clear();

    if (ys->current_search == NULL || ys->current_search[0] == 0) {
        return;
    }

    yed_frame *frame = event->frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) { return; }

    search(graph->get_base());
}

static void color(yed_event *event) {
    yed_frame  *frame;

    frame = event->frame;
    if (frame == NULL) { return; }

    Flame_Graph *graph = graph_in_frame(frame);

    if (graph == NULL) { return; }

    for (auto &info : graph->frame_info[event->row]) {
        yed_attrs attrs = ZERO_ATTR;

        if (ys->current_search != NULL
        &&  ys->current_search[0]
        &&  strstr(info.frame->label, ys->current_search) != NULL) {

            attrs = yed_parse_attrs("&black bg ff00ff");
        } else if (frame == ys->active_frame
               &&  event->row == frame->cursor_line
               &&  info.start_col <= frame->cursor_col && frame->cursor_col <= info.end_col) {

            attrs = yed_parse_attrs("&black bg ff0000");
        } else if (search_match_frames.find(info.frame) != search_match_frames.end()) {
            attrs = yed_parse_attrs("&black bg 7722ff");
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
            snprintf(attr_string, sizeof(attr_string), "fg 000000 bg %02x%02x%02x", r, g, b);
            attrs = yed_parse_attrs(attr_string);
        }

        for (int col = info.start_col; col <= info.end_col; col += 1) {
            yed_eline_combine_col_attrs(event, col, &attrs);
        }
    }
}

static void focus(yed_event *event) {
    yed_buffer *to_buff = NULL;

    if (event->kind == EVENT_FRAME_PRE_SET_BUFFER && event->frame == ys->active_frame) {
        to_buff = event->buffer;
    } else if (event->kind == EVENT_FRAME_PRE_ACTIVATE) {
        if (event->frame == ys->active_frame) { return; }
        to_buff = event->frame->buffer;
    }

    if (graph_from_buffer(to_buff) == NULL) {
        yed_disable_key_map("flame-graph");
    } else {
        yed_plugin_map_bind_key(Self, "flame-graph", ENTER,     "flame-graph-zoom",        0, NULL);
        yed_plugin_map_bind_key(Self, "flame-graph", ESC,       "flame-graph-reset-zoom",  0, NULL);
        yed_plugin_map_bind_key(Self, "flame-graph", BACKSPACE, "flame-graph-return-zoom", 0, NULL);
        yed_plugin_map_bind_key(Self, "flame-graph", ' ',       "flame-graph-frame-info",  0, NULL);
        yed_enable_key_map("flame-graph");
    }
}

static void key(yed_event *event) {
    yed_frame   *frame;
    Flame_Graph *graph = NULL;

    if (!yed_var_is_truthy("flame-graph-bind-mouse")) { return; }

    if (ys->interactive_command) {
        return;
    }

    if (!IS_MOUSE(event->key)
    ||  MOUSE_KIND(event->key) != MOUSE_PRESS) {

        return;
    }

    if (MOUSE_BUTTON(event->key) != MOUSE_BUTTON_LEFT
    &&  MOUSE_BUTTON(event->key) != MOUSE_BUTTON_RIGHT) {

        return;
    }

    frame = ys->active_frame;
    if (frame == NULL) { return; }

    graph = graph_in_frame(frame);
    if (graph == NULL) { return; }

    int row = MOUSE_ROW(event->key);
    int col = MOUSE_COL(event->key);

    if (row < frame->top  || row > frame->top  + frame->height - 1
    ||  col < frame->left || col > frame->left + frame->width  - 1) {

        return;
    }

    row = row - frame->top + frame->buffer_y_offset + 1;
    col = col - frame->left + frame->buffer_x_offset - frame->gutter_width + 1;

    if (MOUSE_BUTTON(event->key) == MOUSE_BUTTON_LEFT) {
        if (graph->get_info(row, col) == NULL) {
            graph->return_zoom(frame->width);
            YEXE("cursor-buffer-end");
            YEXE("cursor-line-begin");
        } else {
            graph->zoom(row, col, frame->width);
            YEXE("cursor-buffer-end");
            YEXE("cursor-line-begin");
        }
        popup.reset();
    } else if (MOUSE_BUTTON(event->key) == MOUSE_BUTTON_RIGHT) {
        popup.reset();
        info_popup(graph, row, col);
    }

    event->cancel = 1;
}

extern "C"
int yed_plugin_boot(yed_plugin *self) {
    char *state_addr_str;
    char  addr_buff[64];

    YED_PLUG_VERSION_CHECK();

    Self = self;

    std::map<void(*)(yed_event*), std::vector<yed_event_kind_t> > event_handlers = {
        { fit,    { EVENT_FRAME_POST_RESIZE, EVENT_TERMINAL_RESIZED,
                    EVENT_FRAME_POST_DELETE, EVENT_FRAME_PRE_SET_BUFFER,
                    EVENT_FRAME_POST_SET_BUFFER                          } },
        { update, { EVENT_FRAME_PRE_UPDATE                               } },
        { color,  { EVENT_LINE_PRE_DRAW                                  } },
        { key,    { EVENT_KEY_PRESSED                                    } },
        { focus,  { EVENT_FRAME_PRE_SET_BUFFER, EVENT_FRAME_PRE_ACTIVATE } },
        { move,   { EVENT_CURSOR_POST_MOVE                               } },
        { draw,   { EVENT_PRE_DIRECT_DRAWS                               } }};

    std::map<const char*, const char*> vars = {
        { "flame-graph-debug-log",  "OFF" },
        { "flame-graph-bind-mouse", "ON"  }};

    std::map<const char*, void(*)(int, char**)> cmds = {
        { "flame-graph",             flame_graph             },
        { "flame-graph-zoom",        flame_graph_zoom        },
        { "flame-graph-reset-zoom",  flame_graph_reset_zoom  },
        { "flame-graph-return-zoom", flame_graph_return_zoom },
        { "flame-graph-frame-info",  flame_graph_frame_info }};

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

    yed_plugin_add_key_map(self, "flame-graph");
    yed_disable_key_map("flame-graph");

    yed_plugin_set_unload_fn(self, unload);

    return 0;
}

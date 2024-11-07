#!/usr/bin/env bash

if [[ $(uname) == "Darwin" ]]; then
    WARN="-Wno-writable-strings -Wno-extern-c-compat"
else
    WARN="-Wno-write-strings -Wno-extern-c-compat"
fi

g++ -o flame_graph.so flame_graph.cpp -std=c++20 ${WARN} $(yed --print-cppflags --print-ldflags)

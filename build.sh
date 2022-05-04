#!/bin/bash

# Written by Philipp Czerner, 2018. Public Domain.
# See LICENSE.md for license information.

EMCC=emcc
GXX=g++
CXXFLAGS="-Wall -Wextra -Werror -Wno-sign-conversion -Wno-unused-variable -Wno-sign-compare -Wno-writable-strings -Wno-write-strings -std=c++14 -fno-exceptions -fno-rtti"
LDFLAGS_GCC="-lGL -lX11 -lXrandr"
LDFLAGS_EMCC="-s BINARYEN_METHOD='native-wasm' -s DISABLE_DEPRECATED_FIND_EVENT_TARGET_BEHAVIOR=1 -s ENVIRONMENT=web -s NO_FILESYSTEM=1 -s DISABLE_EXCEPTION_CATCHING=1"

if [ "$#" -lt 2 ]; then
    echo "Usage:"
    echo "  $0 [gcc|emcc] [debug|release]"
    exit 1
fi;

# This will be passed to emcc, so that they are callable from JavaScript code.
exp_fun=$(sed -n '/^[^#]*OBST_EM_EXPORT/ {s/.*OBST_EM_EXPORT(\([^)]*\).*/"_\1"/;p}' platform_emscripten.cpp |  tr '\n' ',')
exp_fun=${exp_fun:0:(-1)}

if [ "$1" = "gcc" ]; then
    if [ "$2" = "debug" ]; then
        $GXX $CXXFLAGS -fmax-errors=2 -Wno-class-memaccess -Wno-unused-parameter -Wno-comment -ggdb platform_linux.cpp -o obst $LDFLAGS_GCC
    elif [ "$2" = "release" ]; then
        $GXX $CXXFLAGS -fmax-errors=2 -Wno-class-memaccess -Wno-unused-parameter -Wno-comment -O2 platform_linux.cpp -o obst $LDFLAGS_GCC
    else
        echo "Error: second argument must be either debug or release"
    fi;
elif [ "$1" = "emcc" ]; then
    if [ "$2" = "debug" ]; then
        $EMCC $CXXFLAGS -ferror-limit=2 -O0 -gsource-map platform_emscripten.cpp -o obst.js $LDFLAGS_EMCC -s EXPORTED_FUNCTIONS="[$exp_fun]" -s DEMANGLE_SUPPORT=1 -s SAFE_HEAP=0 -s ASSERTIONS=2 -s STACK_OVERFLOW_CHECK=2 --source-map-base "file://`pwd`/"
    elif [ "$2" = "release" ]; then
        $EMCC $CXXFLAGS -ferror-limit=2 -DNDEBUG -O3 platform_emscripten.cpp -o obst.js -Os --closure 1 $LDFLAGS_EMCC -s EXPORTED_FUNCTIONS="[$exp_fun]" -s DEMANGLE_SUPPORT=0 -s SAFE_HEAP=0 -s ASSERTIONS=0 -s STACK_OVERFLOW_CHECK=0
    else
        echo "Error: second argument must be either debug or release"
    fi;
else
    echo "Error: first argument must be either gcc or emcc"
    exit 2
fi;


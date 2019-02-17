#!/bin/bash

EMCC=emcc
GCC=gcc
CXXFLAGS="-Wall -Wextra -Werror -Wno-sign-conversion -Wno-unused-variable -Wno-sign-compare -Wno-writable-strings -ferror-limit=2 -std=c++14 -fno-exceptions -fno-rtti"
LDFLAGS_GCC="-lGL -lX11"

if [ "$#" -lt 2 ]; then
    echo "Usage:"
    echo "  $0 [gcc|emcc] [debug|release]"
    exit 1
fi;

# This will be passed to emcc, so that they are callable from JavaScript code.
exp_fun=$(sed -n '/^[^#]*EM_EXPORT/ {s/.*EM_EXPORT(\([^)]*\).*/"_\1"/;p}' platform_emscripten.cpp |  tr '\n' ',')
exp_fun=${exp_fun:0:(-1)}

if [ "$1" = "gcc" ]; then
    if [ "$2" = "debug" ]; then
        $GCC $CXXFLAGS platform_linux.cpp -o obst $LDFLAGS
    elif [ "$2" = "release" ]; then
        echo TODO
        exit 2
    else
        echo "Error: second argument must be either debug or release"
    fi;
elif [ "$1" = "emcc" ]; then
    if [ "$2" = "debug" ]; then
        $EMCC $CXXFLAGS -O0 -g4 platform_emscripten.cpp -o obst.js -s "BINARYEN_METHOD='native-wasm'" -s EXPORTED_FUNCTIONS="[$exp_fun]" -s DEMANGLE_SUPPORT=1 -s SAFE_HEAP=1 -s ASSERTIONS=2 -s STACK_OVERFLOW_CHECK=2 --source-map-base "file://`pwd`/"
    elif [ "$2" = "release" ]; then
        $EMCC $CXXFLAGS -DNDEBUG -O3 platform_emscripten.cpp -o obst.js -O3 --closure 1 -s "BINARYEN_METHOD='native-wasm'" -s EXPORTED_FUNCTIONS="[$exp_fun]" -s DEMANGLE_SUPPORT=0 -s SAFE_HEAP=0 -s ASSERTIONS=0 -s STACK_OVERFLOW_CHECK=0 -s ENVIRONMENT=web -s NO_FILESYSTEM=1
    else
        echo "Error: second argument must be either debug or release"
    fi;
else
    echo "Error: first argument must be either gcc or emcc"
    exit 2
fi;


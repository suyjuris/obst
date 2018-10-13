#!/bin/bash

emcc -Wall -Wextra -Werror -Wno-sign-conversion -Wno-unused-variable -Wno-sign-compare -ferror-limit=2 -std=c++14 -O0 -g4 obst.cpp -o obst.js -s "BINARYEN_METHOD='native-wasm'" -s EXPORTED_FUNCTIONS='["_main", "_ui_button_create", "_ui_button_move", "_ui_button_removeall", "_ui_button_op", "_ui_button_opr"]' -s DEMANGLE_SUPPORT=1 -s SAFE_HEAP=1 -s ASSERTIONS=2 -s STACK_OVERFLOW_CHECK=2 --source-map-base "file://`pwd`"

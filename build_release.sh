#!/bin/bash

emcc -DNDEBUG -Wall -Wextra -Werror -Wno-sign-conversion -Wno-unused-variable -Wno-sign-compare -ferror-limit=2 -std=c++14 -fno-exceptions -fno-rtti -O3 obst.cpp -o obst.js -O3 --closure 1 -s "BINARYEN_METHOD='native-wasm'" -s EXPORTED_FUNCTIONS='["_main", "_ui_button_create", "_ui_button_help", "_ui_button_move", "_ui_button_removeall", "_ui_button_op", "_ui_button_opr"]' -s DEMANGLE_SUPPORT=0 -s SAFE_HEAP=0 -s ASSERTIONS=0 -s STACK_OVERFLOW_CHECK=0 -s ENVIRONMENT=web -s NO_FILESYSTEM=1

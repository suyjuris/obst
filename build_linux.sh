#!/bin/bash
g++ -Werror -Wall -Wextra -fmax-errors=2 -ggdb platform_linux.cpp -o test -lGL -lX11

#!/usr/bin/python3
# coding: utf-8

# Written by Philipp Czerner, 2018. Public Domain.
# See LICENSE.md for license information.

import sys
import subprocess

if len(sys.argv) <= 1:
    print('Usage:\n  %s file ...\n\nPrint the necessary C code to generate OpenGL stubs.' % sys.argv[0])
    sys.exit(1)

funcs = []
for fname in sys.argv[1:]:
    f = open(fname, 'rb')
    src = b''.join(line for line in f if not line.startswith(b'#include <'))
    f.close()
    p = subprocess.run(['gcc', '-E', '-', '-o', '-'], input=src, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    out = p.stdout
                        
    for line in out.decode('utf-8').split(';'):
        i = -1
        while True:
            i = line.find('gl', i+1)
            if i == -1: break
            if line[i+2:i+3] == 'X': continue
            flag = False
            for j, c in enumerate(line[i:]):
                if c == '(':
                    flag = True
                    break
                elif not c.isalnum():
                    break
            if not flag: continue
            j += i
            name = line[i:j]
            if name not in funcs: funcs.append(name)
    f.close()

funcs_d = {'PFN%sPROC' % i.upper(): i for i in funcs}
    
f = open('/usr/include/GL/glext.h', 'r')
typedefs, symbols, init = [], [], []
for line in f:
    if 'typedef' in line:
        i = line.find('APIENTRYP')
        i1 = i + len('APIENTRYP ')
        j = line.find(')', i)
        name = funcs_d.get(line[i1:j], None)
        if name is None: continue
        name_t = name + '_t'
        typedefs.append(line[:i] + '*' + name_t + line[j:-1])
        symbols.append('' + name_t + ' ' + name + ';')
        init.append('%s = (%s)glXGetProcAddress((u8*)"%s"); assert(%s);' % (name, name_t, name, name))

print('// Auto-generated OpenGL pointer initialisation, see generate_opengl.py')
for i in typedefs: print(i)
for i in symbols: print(i)
print()
print('void _platform_init_gl_pointers() {')
for i in init: print('    ' + i)
print('}')

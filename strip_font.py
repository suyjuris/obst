#!/usr/bin/python2

# Written by Philipp Czerner, 2018. Public Domain.
# See LICENSE.md for license information.

import os
import glob
import sys
import fontforge

ranges = [
    (0x0020, 0x007F, "Basic Latin"),
    (0x00A0, 0x00FF, "Latin-1 Supplement"),
    (0x25A0, 0x25FF, "Geometric Shapes"),
    (0x0100, 0x017F, "Latin Extended-A"),
    (0x0180, 0x024F, "Latin Extended-B"),
    (0x0250, 0x02AF, "IPA Extensions"),
    (0x0370, 0x03FF, "Greek and Coptic"),
    (0x1E00, 0x1EFF, "Latin Extended Additional"),
    (0x1F00, 0x1FFF, "Greek Extended"),
    (0x2000, 0x206F, "General Punctuation"),
    (0x2070, 0x209F, "Superscripts and Subscripts"),
    (0x20A0, 0x20CF, "Currency Symbols"),
    (0x2150, 0x218F, "Number Forms"),
    (0x2190, 0x21FF, "Arrows"),
    (0x2200, 0x22FF, "Mathematical Operators"),
]
counts = [0 for i in ranges]

chars = set()
    
for fname in glob.glob('*.[ch]pp'):
    f = open(fname, 'rb')
    chars |= set(f.read().decode("utf-8"));
    f.close()

for fname in reversed(glob.glob('fonts_original/*.ttf')):
    font = fontforge.open(fname)

    if fname.endswith('DejaVuSans.ttf'):
        font.selection.all()
        for i in font.selection.byGlyphs:
            for j, (a,b,name) in enumerate(ranges):
                if a <= i.unicode <= b: counts[j] += 1
            font.selection[i.encoding] = not any(a <= i.unicode <= b for a,b,name in ranges)
        for j, (a,b,name) in enumerate(ranges):
            print name, counts[j]
    else:
        font.selection.none()
        for c in chars:
            font.selection[ord(c)] = True
        font.selection.invert()

    for i in font.selection.byGlyphs:
        font.removeGlyph(i)

    font.generate('fonts_stripped' + fname[len('fonts_original'):])
    font.close()

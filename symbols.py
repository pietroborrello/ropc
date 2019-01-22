#!/usr/bin/env python

from pwn import *
import sys
elf = ELF(sys.argv[1], False)
sym = elf.symbols
s = ''
for f in sym:
    if(str(f) != ''):
        s+= str(f.replace('__isoc99_', '')) + '>' + str(sym[f]) + ' '
    
print s.strip()

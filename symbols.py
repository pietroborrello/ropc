#!/usr/bin/env python

from pwn import *
a = ELF('a.out', False)
sym = a.symbols
s = ''
for f in sym:
    if(str(f) != ''):
        s+= str(f.replace('__isoc99_', '')) + '>' + str(sym[f]) + ' '
    
print s.strip()
    
    
    
    
 

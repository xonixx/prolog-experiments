
from decimal import *

getcontext().prec=15

print sum(Decimal(f.replace(',','.') or '0') for f in open('numbers_large.txt').read().split(' '))
:-write('# xonix
o,n=map(chr,(39,10));oo=o+o
p='':-write(%s%s%s).''
q=''# xonix%so,n=map(chr,(39,10));oo=o+o%sp=%s%s%s%sq=%s%s%s%sprint p%%(o,q%%(n,n,oo,p,oo,n,oo,q,oo,n),o)''
print p%(o,q%(n,n,oo,p,oo,n,oo,q,oo,n),o)').

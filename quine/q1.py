# xonix
d="%"+"%"
def f(s,a):return s.replace(d,a)
q='# xonix\nd="%"+"%"\ndef f(s,a):return s.replace(d,a)\nq=%%\nprint f(q,repr(q))'
print f(q,repr(q))

# xonix
L=19;B,Q,N,q,n=map(chr,(36,81,78,39,10))
X='import sys;sys.stdout.write(%s%s%s.replace(chr(36)+chr(81)+chr(36),chr(39)).replace(chr(36)+chr(81),chr(36)).replace(chr(36)+chr(78)+chr(36),chr(10)).replace(chr(36)+chr(78),chr(36)))'
Y='# xonix%sL=%s;B,Q,N,q,n=map(chr,(36,81,78,39,10))%sX=%s%s%s%sY=%s%s%s%sE="""%s""";exec E%simport sys;sys.stdout.write(b())'
E="""def b(l=L):
  if l==L: Ql=q
  else: Ql=B+Q*(L-l)+B;Nl=B+N*(L-l)+B
  if l>0: return X%(Ql,b(l-1),Ql)
  else: return Y%(Nl,str(L),Nl,Ql,X,Ql,Nl,Ql,Y,Ql,Nl,E.replace(n,Nl),Nl)""";exec E
import sys;sys.stdout.write(b())
# xonix
L=3;B,Q,N,q=map(chr,(36,81,78,39))
X='import sys;sys.stdout.write(%s%s%s.replace(chr(36)+chr(81)+chr(36),chr(39)).replace(chr(36)+chr(81),chr(36)).replace(chr(36)+chr(78)+chr(36),chr(10)).replace(chr(36)+chr(78),chr(36)))'
Y='# xonix%sL=%s;B,Q,N,q=map(chr,(36,81,78,39))%sX=%s%s%s%sY=%s%s%s%sE="""%s""";exec E%simport sys;sys.stdout.write(b())'
E="""def b(l=L):
	if l==L: Ql=q
	else: Ql=B+Q*(L-l)+B;Nl=B+N*(L-l)+B
	if l>0: return X%(Ql,b(l-1),Ql)
	else: return Y%(Nl,str(L),Nl,Ql,X,Ql,Nl,Ql,Y,Ql,Nl,repr(E).replace(q,str()),Nl)""";exec E
import sys;sys.stdout.write(b())
	
#print	
#print '"""%s"""'%repr(E)	
#import sys;sys.stdout.write(X%(q,Y%(n,n,qq,X,qq,n,qq,Y,qq,n),q))

#def r(s):import re;return re.sub(s,'~\dn',lambda m: )
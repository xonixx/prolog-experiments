# xonix
L=3

#q=chr(39) # '

#s=chr(92) # \
#n=chr(110) # n
#nl=s+n # \n
B=chr(36) # $
Q=chr(81) # Q
N=chr(78) # N

#BQ=B+Q
#BQB=BQ+B

#BN=B+N
#BNB=BN+B

X='import sys;sys.stdout.write(%s%s%s.replace(chr(36)+chr(81)+chr(36),chr(39)).replace(chr(36)+chr(81),chr(36)).replace(chr(36)+chr(78)+chr(36),chr(10)).replace(chr(36)+chr(78),chr(36)))'
Y='# xonix%sL=%s;q,n,R,O=map(chr,(39,10,113, 110))%sX=%s%s%s%sY=%s%s%s%s'

def b(l=L):
	if l==L: Ql=chr(39)
	else: Ql=B+Q*(L-l)+B;Nl=B+N*(L-l)+B
	if l>0: return X%(Ql,b(l-1),Ql)
	else: return Y%(Nl,str(L),Nl,Ql,X,Ql,Nl,Ql,Y,Ql,Nl)
import sys;sys.stdout.write(b())
	
#import sys;sys.stdout.write(X%(q,Y%(n,n,qq,X,qq,n,qq,Y,qq,n),q))

#def r(s):import re;return re.sub(s,'~\dn',lambda m: )
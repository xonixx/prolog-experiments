
a(R,A):-
	(   (R=v;R=t)
	->  A=1
	;   A=0
	).

v(R,V):-
	(   (R\=v,R\=y)
	->  V=1
	;   V=0
	).

d(D):-
	(   (a(_,A),v(_,V),A+V=:=1)
	->  D=1
	;   D=0
	).

y(Y):-
	(   d(0)
	->  Y=1
	;   Y=0
	).

/*	
a(R,A):-  (R=v;R=t),!,A=1;A=0.

v(R,V):-  (R\=v,R\=y),!,V=1;V=0.

d(D):-   a(_,A),v(_,V),A+V=:=1,!,D=1;D=0.

y(Y):-  d(0),!,Y=1;Y=0.
*/	
solve(R):-
	member(R,[a,v,d,y,t]),
	a(R,A),v(R,B),d(D),y(Y),A+B+D+Y>=3.

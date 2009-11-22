
fact(N, Res) :-
	(   N =< 1
	->  Res = 1
	;   N1 is N - 1,
	    fact(N1, Res1),
	    Res is N * Res1
	).

:- op(700, xfx, ?<=).
:- op(700, xfx, ?>).
:- op(700, xfx, ??=).
:- dynamic [eq/2]. 

get1(A, ARes) :-
	(   number(A)
	->  ARes = A
	;   atom(A)
	->  eq(A, ARes)
	;   A =.. [Op, A1, A2],
	    get1(A1, A01),
	    get1(A2, A02),
	    A0 =.. [Op, A01, A02],
	    ARes is A0
	),
	!.

get1(A, _) :-
	throw(unknown(A)).

set(A, Val) :-
	get1(Val, Val1),
	(   atom(A)
	->  retractall(eq(A, _)),
	    assert(eq(A, Val1))
	;   var(A)
	->  A = Val1
	).

A ??= B :-
	set(A, B).

A ?<= B :-
	get1(A, A1),
	get1(B, B1),
	A1 =< B1.

A ?> B :-
	get1(A, A1),
	get1(B, B1),
	A1 > B1.

while(Condition, Action) :-
	repeat,
	(   Condition
	->  Action,
	    fail
	;   !
	).

for(Start, Condition, Step, Action) :-
	Start,
	while(Condition, 
	      (	  Action,
		  Step
	      )).

if(Condition, Action, ElseAction) :-
	(   Condition
	->  Action
	;   ElseAction
	).

if(Condition, Action) :-
	if(Condition, Action, true).

fact1(N, Res) :-
	n ??= N,
	if(n ?<= 1,
	   (   Res = 1
	   ),
	   (   r ??= 1,
	       while(n ?> 1,
		  (   r ??= r * n,
		      n ??= n - 1
		  )),
	       Res ??= r
	   )).
	    

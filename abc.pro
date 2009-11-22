
letters(Letter, Num) --> letters(Letter, 0, Num).

letters(Letter, N1, N) --> 
	{N1<N}, !, 
	[Letter], 
	{N2 is N1 + 1},
	letters(Letter, N2, N).
letters(_,N,N) --> [].

l(L,N) --> [L], !, l(L,N1), {N is N1 + 1}.
l(_,0) --> !, [].

correct -->
	l(a,N), {N mod 2 =:= 0},
	l(b,Nb), {Nb mod 2 =:= 1},
	l(c,N).

tst(Lst) :- phrase(correct, Lst, []).

% check
test :-
	tst([a,a,a,a,b,b,b,c,c,c,c]),
	tst([a,a,b,c,c]),
	tst([b,b,b]),
	tst([b]),
	tst([a,a,a,a,a,a,b,c,c,c,c,c,c]),
	\+ tst([a,a,b,b,c,c]),
	\+ tst([a,a,a,b,b,b,c,c]).


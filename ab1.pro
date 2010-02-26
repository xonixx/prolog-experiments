:-use_module(library(clpfd)).

n(L, N) --> [L], {N1 #= N-1}, !, n(L, N1).
n(_, 0) --> [].

abc -->	n(a, N),
	{N1 is N + 1,
	 N2 is N + 2
	},
	n(b, N1),
	n(c, N2).

p(X) :- phrase(abc, X).

%%	
%%	test
%%	

test :-
	p([b,c,c]),
	p([a,b,b,c,c,c]),
	p([a,a,b,b,b,c,c,c,c]),
	\+ p([a,a,a,b,b,b,b]),
	\+ p([a]),
	\+ p([a,b,b,c]).


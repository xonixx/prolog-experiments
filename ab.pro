:-use_module(library(clpfd)).

n(L, N) -->
	[L],
	{N1 #= N-1
	},
	!, n(L, N1).
n(_, 0) --> [], !.

ab -->
	n(a, N),
	{N1 is N + 1
	},
	n(b, N1).

p(X) :- phrase(ab, X).

test :-
	p([b]),
	p([a,b,b]),
	p([a,a,b,b,b]),
	p([a,a,a,b,b,b,b]),
	\+ p([a]),
	\+ p([a,b]),
	\+ p([a,a,b,b]),
	\+ p([a,b,b,c]).


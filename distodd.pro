
:- use_module(library(clpfd)).

all_odd([]) :-!.
all_odd([H | T]) :-
	H mod 2 #= 1,
	all_odd(T).

solve(N,L) :-
	N2 is floor(sqrt(N)),
	Len in 1..N2,
	label([Len]),
	
	length(L, Len),
	
	L ins 1..N,
	
	all_different(L),
	all_odd(L),
	
	sum(L,#=,N),
	
	label(L),
	
	% only show sorted sets
	sort(L,L).

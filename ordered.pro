
ordered([]) :-!.
ordered([_]):-!.
ordered([A,B|T]) :-
	A =< B,
	!,
	ordered([B|T]).


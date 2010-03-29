/*
:- use_module(library(clpfd)).

make_lst(N, L, Vars) :-
	length(Vars, L),
	Vars ins N.

do(_, 0) :-!.
do(Lst, N) :-
	Lst = [_|T],
	Lst1 = [2|T],
	N1 is N-1,
	do(Lst1, N1).

*/

freq(Lst, Assoc) :-
	empty_assoc(E),
	freq(Lst, E, Assoc).

freq([H | T], E0, E1) :-
	(   get_assoc(H, E0, V)
	->  V1 is V + 1,
	    put_assoc(H, E0, V1, E)
	;   put_assoc(H, E0, 1, E)
	),
	freq(T, E, E1).
freq([],E,E).



gen_rnd_lst(0, _) --> [], !.
gen_rnd_lst(N, R) --> [H], {H is random(R), N1 is N - 1}, !, gen_rnd_lst(N1, R).

ask(_, 0, _) :-!.
ask(Assoc, N, M) :-
	R is random(M),
	(   get_assoc(R, Assoc, _)
	;   true
	),
	N1 is N - 1,
	!,
	ask(Assoc, N1, M).

tst(Lst) :-
	N = 1000000, M = 200,

	format('Generating ~D randoms from 0 to ~D...~n', [N, M]),
	time(phrase(gen_rnd_lst(N, M), L)),

	writeln('Making freq hash-table...'),
	time(freq(L, Assoc)),

	format('Performing ~D selects...~n', [N]),
	time(ask(Assoc, N, M)),

	writeln('Converting to list of pairs...'),
	time(assoc_to_list(Assoc, Lst)).



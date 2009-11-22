:- dynamic saved/3, len_saved/2, max_saved/1.

f(N, R) :-
	(   0 is N mod 2
	->  R is N//2
	;   R is 3 * N + 1
	).

len(1, 1) :-!.
len(N, L) :-
	len_saved(N, L), !;
	f(N, R),
	len(R, L1),
	L is L1 + 1
	.

save_all_lens :-
	retractall(len_saved(_, _)),
	save_len(1).

save_len(N) :-
	N =< 1000000,
	log(N),
	len(N, L),
	asserta(len_saved(N, L)),
	Nnext is N + 1,
	save_len(Nnext).

log(N) :-
	(   0 is N mod 10000
	->  write(N),nl
	;   true
	).

:- asserta(max_saved(1,1)).
max1 :-
	len_saved(N, L),
	log(N),
	max_saved(_, M),
	(   M < L
	->  retractall(max_saved(_, _)),
	    asserta(max_saved(N, L))
	),
	fail.
max1 :-
	write('Max: '),max_saved(Nm, M), write(Nm),write(' '),write(M).

do :-
	(   save_all_lens; true),
	max1.
	
	
	














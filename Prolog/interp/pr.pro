
:- module(pr, [wr/1, pr/1,
	      dbg/0, nodbg/0, dbg/1
	      ]).
%:- module_transparent wr/1.
:- dynamic [do_dbg/0].

dbg :-
	assert(do_dbg).

nodbg :-
	retractall(do_dbg).

dbg(Smth) :-
        (   do_dbg
        ->  pr(['DEBUG: ' | Smth])
        ;   true
        ).

wr([]) :-!.
wr([H | T]):-
        write(H),wr(T),!.
wr(A) :-
	write(A).

pr(A) :- wr(A),nl.


% введем необходимые операторы
:- op(100, fy, ~). % не
:- op(110, yfy, &). % и
:- op(120, yfy, v). % или
:- op(130, xfy, =>). % следует
:- op(140, xfy, <=>). % эквивалентно


is_dnf(A v B) :-
	is_dnf(A),
	is_dnf(B),
	!.

is_dnf(A) :-
	is_k(A).

is_k(A & B) :-
	is_k(A),
	is_k(B),
	!.

is_k(A) :- atom_(A), !.
is_k(~A) :- atom_(A).

atom_(A) :-
	atom(A),
	atom_to_chars(A, Chars),
	phrase(many_alpha,Chars).

many_alpha --> [H], {code_type(H, alpha)}, !, many_alpha.
many_alpha --> [].

:- op(1200, xfx, :--).
:- meta_predicate :--(2,?,?).

replace_terms([H | T], From, To, [H1 | T1]) :-
	replace_term(H, From, To, H1), !,
	replace_terms(T, From, To, T1).
replace_terms([], _, _, []).

replace_term(Term, From, To, To) :- nonvar(Term), Term = From, !.
replace_term(Term, _, _, Term) :- (atomic(Term); var(Term)), !.
replace_term(Term, From, To, Res) :-
	Term =.. Terms,
	replace_terms(Terms, From, To, Terms1),
	Res =.. Terms1.
/*
term_expansion(A, _):-
	format('A: ~w~n',[A]), fail.*/

term_expansion(H :-- B, Res) :-
	%writeln([H, B]),
	H =.. HA,
	append(HArgs, [...], HA),

	findall(H1 :- B1, (member(N, [0,1,2,3,4,5]),
			   length(L, N),
			   replace_term(B, ..., L, B1),
			   append(HArgs, L, H1A),
			   H1 =.. H1A
			  ), Res).

a(B, ...) :--
	format('predicate a called with B=~w and ...=~w', [B, ...]).

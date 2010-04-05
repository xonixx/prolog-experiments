
:- use_module(library(http/dcg_basics)).

nl --> "\r\n".
nl --> "\n".

n_lines([L | LL]) --> string(L), nl, !, n_lines(LL).
n_lines([]) --> [].

write_lines([L | LL]) :- format('~s~n', [L]), !, write_lines(LL).
write_lines([]).

main(InF, OutF) :-
	phrase_from_file(n_lines(Lines), InF),
	reverse(Lines, Reversed),
	tell(OutF),
	write_lines(Reversed),
	told.

%?- main('in.txt', 'out.txt').

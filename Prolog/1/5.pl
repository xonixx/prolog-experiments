
two_letters([A, B | _], [A, B]).

% 44 ?- how_much("abcaasdadbcdksdcbbcbcbc", "bca",R).
%
% R = 5

how_much(X, Y, Res) :-
	two_letters(Y, L),
	how_much_1(X, L, Res).

contains(A1, A2) :-
	append([_, A2, _], A1).

how_much_1(X, L, Res) :-
	append([F, L, T], X),
	\+contains(F, L),
	how_much_1(T, L, R1),
	Res is R1 + 1.

how_much_1(_, _, 0).

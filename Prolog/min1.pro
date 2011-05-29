
:- use_module(library(clpr)).

solve(X1, X2, X3, _Max) :-
	{X1 >= 0, X1 =< 10, 
	 X2 >= 0, X2 =< 10, 
	 X3 >= 0, X2 =< 10,
	 1.5*X1 + 2.5*X2 + 6*X3 =< 20
	}, 
	maximize(17*X1 + 30*X2 + 75*X3)
	%% inf(17*X1 + 30*X2 + 75*X3, Max)
	.

:- use_module(library(clpfd)).
solve1(Vars, Res) :-
	Vars = [X1, X2, X3], Vars ins 0..10,
	3*X1 + 5*X2 + 12*X3 #=< 40,
	F = 17*X1 + 30*X2 + 75*X3,
	once(labeling([max(F)], Vars)),
	Res is F.

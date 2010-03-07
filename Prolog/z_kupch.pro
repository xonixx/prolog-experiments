
:- use_module(library(clpq)).

solve(Res) :-
	Res = [olimpiada/_/CupsO, sosipatra/_/CupsS, polixena/_/CupsP],
	{CupsO + 5 = CupsS + CupsP,
	 CupsS + 9 = CupsO + CupsP},

	select(_/karpovna/11, Res, L1),
	select(_/titovna/CupsT, L1, [_/uvarovna/_]), CupsT mod 3 =:= 0.







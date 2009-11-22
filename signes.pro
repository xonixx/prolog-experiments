
% ((((1?2)?3)?4)?5)?6
solve(Formula) :-
	Signs = [+, -, *, //],
	member(Op1, Signs),
	member(Op2, Signs),
	member(Op3, Signs),
	member(Op4, Signs),
	member(Op5, Signs),
	A =.. [Op1, 1, 2],
	B =.. [Op2, A, 3],
	C =.. [Op3, B, 4],
	D =.. [Op4, C, 5],
	Formula =.. [Op5, D, 6],
	35 =:= Formula.

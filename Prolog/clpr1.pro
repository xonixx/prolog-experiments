
:-use_module(library(clpr)).

solve1 :-
	Vars = [A, B, C, D, E, F, G, H, W],
	{A + B + C >= 100,
D + E + F >= 50,
G + H     >= 30,
8 * B + 2 * E + 7 * H > 620,
	W = 5 * 0.8 * A + 8 * 1.0 * B + 5 * C + 3 * 0.8 * D + 2 * 1.0 * E + 2 * F + 6 * 0.8 * G + 7 * 1.0 * H

%Linear function:

	},
	(
	%writeln(W),
	dump(Vars, [a,b,c,d,e,f,g,h,w], Cc),
	format('1 ~w~n', [Cc]),
	inf(W, I), writeln(I),
	 %minimize(W), format('Res W=~w, Vars=~w~n',[W,Vars]),
	 !

	;
	dump(Vars, [a,b,c,d,e,f,g,h,w], Cc),
	format('2 ~w~n', [Cc])
	).

solve2 :-
	Vars = [A, B],
	{
	 A + B + C > 10,
	 A > 5,
	 W = A + 2 * B + C
	},
	dump(Vars, [a,b], Cc), writeln(Cc),
	inf(W, I), writeln(I).


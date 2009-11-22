
solve(S) :-
	S = [m/M, b/B, l/L, d/D],
	permutation([1,2,3,4], [M, B, L, D]),
	(   D = 2
	;   B = 3
	),
	(   M = 2
	;   B = 1
	),
	(   L = 2
	;   B = 4
	).

solve1(S) :-
	S = [m/M, b/B, l/L, d/D],
	permutation([1,2,3,4], [M, B, L, D]),
	(   D = 2
	;   B = 3
	),
	(   M = 2
	;   L = 1
	),
	(   L = 2
	;   B = 4
	).

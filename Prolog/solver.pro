:-use_module(library(clpfd)).

make_var_pairs(Vp) -->
	[A],
	{member(A/_, Vp)},
	!,
	make_var_pairs(Vp).
make_var_pairs(_) --> [].

to_vars([H | T], Vp) -->
	[A],
	{member(A/H, Vp)},
	!,
	to_vars(T, Vp).
to_vars([], _) --> [].

last(L, N, Ln) :-
	length(L, Len),
	(   Len > N
	->  length(Ln, N),
	    append(_, Ln, L)
	;   Ln = L
	).

n([H],  H):-!.
n([H | T], R) :-
    n(T, Rt),
    length(T, L),
    Ten is 10^L,
    R = H * Ten + Rt.

%set_

check([S1, S2, S3], Vp, [V1, V2, V3], N) :-
	last(V1, N, V1n),
	last(V2, N, V2n),
	last(V3, N, V3n),

	last(S1, N, S1n),
	last(S2, N, S2n),
	last(S3, N, S3n),

	append([S1n, S2n, S3n], S),
	list_to_set(S, Ss),

	phrase(to_vars(Vars, Vp), Ss),

	%append([V1n, V2n, V3n], VV),
	Vars ins 0..9,
	all_different(Vars),

	maplist(n, [V1n,V2n,V3n], [V1e,V2e,V3e]),
	%writeln([V1e,V2e,V3e]),

	V1e * V2e mod 10 ^ N #= V3e %,

	%append([V1, V2, V3], V123),

	%format('pre: ~w~n', V123),
	%label(V123)
	%,writeln(V123)
	.

solve(S1 * S2 = S3, R1 * R2 = R3) :-
	append([S1, S2, S3], S),
	list_to_set(S, Ss),

	length(Ss, SsLen),
	length(Vp, SsLen),

	phrase(make_var_pairs(Vp), Ss),

	phrase(to_vars(Vars, Vp), Ss),
	writeln(Vp),
	writeln(Vars),

	Vars ins 0..9,
	all_different(Vars),

	phrase(to_vars(V1, Vp), S1),
	phrase(to_vars(V2, Vp), S2),
	phrase(to_vars(V3, Vp), S3),

	V1_2_3 = [V1, V2, V3],
	S1_2_3 = [S1, S2, S3],
	writeln(V1_2_3),

	%N = 5,
	check(S1_2_3, Vp, V1_2_3, 1),
	%format('1: ~w~n', V1_2_3),
	check(S1_2_3, Vp, V1_2_3, 2),
	%format('2: ~w~n', V1_2_3),
	check(S1_2_3, Vp, V1_2_3, 3),
	%format('3: ~w~n', V1_2_3),
	check(S1_2_3, Vp, V1_2_3, 4),
	%format('4: ~w~n', V1_2_3),

	append(V1_2_3, V123),
	label(V123),

	%n(V1, R1), n(V2, R2), n(V3, R3),
	maplist(n, [V1,V2,V3], [R1e,R2e,R3e]),
	R1e * R2e #= R3e,
	label(Vars),
	maplist(is, [R1, R2, R3], [R1e, R2e, R3e])

	%,writeln(V1 * V2 = V3)
	.


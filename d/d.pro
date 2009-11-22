

dd([H | T], [Dh | Dt]) :-
	d(H, Dh),
	dd(T, Dt).

dd([], []).

d(A + B, Da + Db) :-
	dd([A, B], [Da, Db]).

d(A * B, Da * B + A * Db) :-
	dd([A, B], [Da, Db]).

d(A / B, (B * Da - A * Db) / B^2) :-
	dd([A, B], [Da, Db]).

d(A, 0) :-
	number(A).

d(e ^ X, Dx * e ^ X) :- 
	d(X, Dx).

d(x ^ N, N * x ^ N1) :-
	number(N),
	N1 is N - 1.

d(ln(X), Dx / X) :- d(X, Dx).
d(sin(X), Dx * cos(X)) :- d(X, Dx).
d(cos(X), -1 * Dx * sin(X)) :- d(X, Dx).
d(tg(X), Dx / cos(X)^2) :- d(X, Dx).
d(ctg(X), -1 * Dx / sin(X)^2) :- d(X, Dx).
	
d(A, Da) :-
	equal(A, A1),
	d(A1, Da).

equal(X ^ Y, e ^ (ln(X) * Y)).
equal(X - Y, X + (-1 * Y)).
equal(x, x ^ 1).
equal(exp(X), e ^ X).
equal(log(C, X), ln(X) / ln(C)).
	
d1(A, Da) :- 
	simplify(A, Sa), 
	d(Sa, Da0), 
	simplify(Da0, Da), !.

simplify0Sign(N, M, Sign, Nm) :-
	numbers([N, M]),
	NSignM =.. [Sign, N, M],
	Nm is NSignM.

simplify0(_ ^ 0, 1) :-!.
simplify0(1 ^ _, 1) :-!.
simplify0(X ^ 1, X) :-!.
simplify0(A + 0, A) :-!.
simplify0(0 + A, A) :-!.
simplify0(1 * A, A) :-!.
simplify0(A * 1, A) :-!.
simplify0(0 * _, 0) :-!.
simplify0(_ * 0, 0) :-!.
simplify0(N * M, Nm) :- simplify0Sign(N, M, *, Nm), !.
simplify0(N / M, Nm) :- simplify0Sign(N, M, /, Nm), !.
simplify0(N + M, Nm) :- simplify0Sign(N, M, +, Nm), !.
simplify0(N - M, Nm) :- simplify0Sign(N, M, -, Nm), !.

simplify0(x * x, x^2) :-!.
simplify0(x * (N*x), N * x^2) :-!.
simplify0(x * N, N * x) :- number(N), !.

simplify0(x * x^N, x^N1) :- add(1, N, N1), !.
simplify0(x * (M*x^N), M*x^N1) :- add(1, N, N1), !.
simplify0(N * (M*X), Nm * X) :- mul(N, M, Nm), !.

simplify0(x^A * (M*x^B), M*x^Ab) :- add(A, B, Ab), !.
simplify0((N*x^A) * (M*x^B), Nm*x^Ab) :- 
	mul(N, M, Nm),
	add(A, B, Ab),
	!.

simplify0(C * X ^ N / X ^ M,C * X ^ Nm) :- simplify0(N - M, Nm), !.
simplify0(e ^ (ln(X) * Y), X ^ Y) :-!.

simplify0(x + x, 2 * x) :-!.
simplify0(x + N*x, N1*x) :- add(1, N, N1), !.
simplify0(N*x + x, N1*x) :- add(1, N, N1), !.
simplify0(N*x + M*x, Nm*x) :- add(N, M, Nm), !.

simplify0(A, A).

%simplify0(Sum, R) :- aggreg(+, Sum, R).
%simplify0(Mul, R) :- aggreg(*, Mul, R).
	
add(A, B, Ab) :-
	numbers([A, B]),
	Ab is A + B.

mul(A, B, Ab) :-
	numbers([A, B]),
	Ab is A * B.



aggreg(Sign, Expr, R) :-
	to_arr(Sign, Expr, Arr),
	addNums(0, N, Arr, Arr1),
	to_arr(Sign, Sum1, Arr1),
	R =.. [Sign, N, Sum1].

addNums(N1, N3, [N | T], Trest) :-
	number(N),
	N2 is N1 + N,
	addNums(N2, N3, T, Trest), 
	!.
addNums(N1, N2, [H | T], [H | Trest]) :-
	addNums(N1, N2, T, Trest),
	!.
addNums(N, N, [], []).

simplifySign(A, B, Sign, R) :-
	simplify(A, Sa),
	simplify(B, Sb),
	S =.. [Sign, Sa, Sb],
	simplify0(S, R).

numbers([N | Ns]) :- number(N), numbers(Ns).
numbers([]).

simplify(N1 * N2 * C, R) :-
	numbers([N1, N2]),
	N is N1 * N2,
	simplify(C, Sc),
	simplify(N * Sc, R).

simplify(N1 * (N2 * C), R) :- simplify(N1 * N2 * C, R).
simplify((X ^ A) ^ B, X ^ Ab) :-
	simplify(A * B, Ab).

simplify(A + B, R) :- simplifySign(A, B, +, R).
simplify(A * B, R) :- simplifySign(A, B, *, R).
simplify(A / B, R) :- simplifySign(A, B, /, R).
simplify(A ^ B, R) :- simplifySign(A, B, ^, R).
simplify(A - B, R) :- simplify(A + -1 * B, R).

simplify(A, R) :-
	simplify0(A, R).
	
simplify(A, A). % if can't simplify - leave as is.


to_arr(Op, Expr, R) :-
	(   var(R)
	->  Expr =.. [Op, Expr1, H],
	    to_arr(Op, H, H1),
	    (   to_arr(Op, Expr1, T)
	    ;   T = [Expr1]
	    ),
	    append(T, H1, R)
	;   [H1, H2 | T] = R,
	    H =.. [Op, H1, H2],
	    to_arr(Op, Expr, [H | T])
	),
	!.

/**
to_arr(Op, Expr, [H1, H2 | T]) :-
	    H =.. [Op, H1, H2],
	    to_arr(Op, Expr, [H | T]),
	    !.
**/

to_arr(_, E, [E]).

/*
findall(M1,
		 (   member(M, SumList),
		     simplify_sum_mul(*, M, M1)
		 ),
		SumList1
	       ),

*/
simplify_mul_expr(Expr, ResultExpr) :-
	to_arr(*, Expr, SumList),
	simplify_sum_mul(*, SumList, ResultSumList),
	to_arr(*, ResultExpr, ResultSumList).


simplify_sum_expr(Expr, ResultExpr) :-
	to_arr(+, Expr, SumList),
	maplist(simplify_mul_expr, SumList, SumList1),
	simplify_sum_mul(+, SumList1, ResultSumList),
	to_arr(+, ResultExpr, ResultSumList).
	

simplify_sum_mul(Op, SumList, ResultSumList) :-
	(   select(E1, SumList, S1),
	    select(E2, S1, S2),
	    E12 =.. [Op, E1, E2],
	    simplify0(E12, E), E \== E12
	->  simplify_sum_mul(Op, [E | S2], ResultSumList)
	), !.

simplify_sum_mul(_, S, S).
	










merge(Nums, Ops, Res) :-
	merge(Nums, Ops, [], 0, Res).

merge(Nums, [O | Oo], Rr, A, Res) :-
	(   A > 1,
	    A1 is A - 1,
	    merge(Nums, Oo, [O | Rr], A1, Res)
	);
	(   Nums = [N | Nn],
	    A1 is A + 1,
	    merge(Nn, [O | Oo], [N | Rr], A1, Res )
	).
merge([], _, Res, _, Res).

merged_to_formula(Merged, Formula) :-
	reverse(Merged,Reversed),
	m_to_f(Reversed, [], Formula).

m_to_f([H | T], S, F) :-
	(   memberchk(H, [+,-,*,/])
	->  S = [A1,A2|Ss], F0 =.. [H, A2, A1], m_to_f(T, [F0|Ss], F)
	;   m_to_f(T, [H | S], F)
	).
m_to_f([], [F], F).

all_ops([Op | Ops]) :-
	member(Op, [+,-,*,/]),
	all_ops(Ops).
all_ops([]).

all_formulas(Nums, Formula) :-
	permutation(Nums, NumsP_0),

	(   NumsP = NumsP_0
	;   [A|B] = NumsP_0, NumsP = [-A|B] % also consider first negative
	),
	length(Nums,N),
	NOps is N - 1,

	length(OpsP, NOps),
	all_ops(OpsP),

	merge(NumsP, OpsP, Merged),
	merged_to_formula(Merged, Formula).

solve(Nums, Res, F) :-
	all_formulas(Nums, F),
	catch(F=:=Res, _, fail). % zero division

solve_all(Nums, Res, FF) :-
	setof(F,solve(Nums, Res, F), FF).

solve(FF) :- solve_all([1,3,4,6],24,FF).

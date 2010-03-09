
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
	merged_to_formula0(Reversed, Formula).

merged_to_formula0(L, F) :-
	m_to_f(L, [], F).

m_to_f([H | T], S, F) :-
	(   memberchk(H, [+,-,*,/])
	->  S = [A1,A2|Ss], F0 =.. [H, A2, A1], m_to_f(T, [F0|Ss], F)
	;   m_to_f(T, [H | S], F)
	).
m_to_f([], [F], F).

all_formulas(Nums, Formula) :-
	Ops = [+,-,*,/],
	permutation(Nums, NumsP),
	%permutation(Ops, OpsP),
	OpsP = [Op1, Op2, Op3],
	member(Op1, Ops),
	member(Op2, Ops),
	member(Op3, Ops),
	merge(NumsP, OpsP, Merged),
	merged_to_formula(Merged, Formula).

solve(FF) :-
	findall(F,
		(   all_formulas([1,3,4,6], F),
		    catch(F=:=24, _, fail)
		), FF).

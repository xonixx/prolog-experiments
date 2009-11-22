
to_aggr(Aggr, {Aggr, L}, {Aggr, L}) :-!.
to_aggr(Aggr, A, {Aggr, [A]}) :-!.

rewrite0(Aggr, A, B, {Aggr, L}) :-
	rewrite(A, A1),
	rewrite(B, B1),
	to_aggr(Aggr, A1, {Aggr, L1}),
	to_aggr(Aggr, B1, {Aggr, L2}),
	append(L1, L2, L).

:- op(200, fy, /).

% det
rewrite(-A, -R) :- rewrite(A, R), !.
rewrite(/A, /R) :- rewrite(A, R), !.
rewrite(A+B, R) :- rewrite0(sum, A, B, R), !.
rewrite(A*B, R) :- rewrite0(mul, A, B, R), !.
rewrite(A-B, R) :- rewrite(A + (-B), R), !.
rewrite(A/B, R) :- rewrite(A * (/B), R), !.

% last
rewrite(A, A) :-!.

numbers([N | Ns]) :- number(N), numbers(Ns).
numbers([]) :-!.

s0(A + B, R) :-
	numbers([A, B]),
	R is A + B.

s({sum, L}, {sum, Lres}) :-
	(   append(L1, [A | L2], L),
	    append(L21, [B | L22], L2),
	    s0(A + B, R)
	->  append(L21, L22, L23),
	    append(L1, [R | L23], Lnew),
	    s({sum, Lnew}, {sum, Lres})
	;   Lres = L
	),
	!.
	
	    

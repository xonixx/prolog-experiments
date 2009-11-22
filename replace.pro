replace(L1,L2,L3,L4) :-
	(   var(L1)
	;   var(L2)
	) 
	-> replace0(L4,L3,L2,L1)
	;  replace0(L1,L2,L3,L4).

replace0(L1, L2, L3, L4) :-
    append([A, L2, B],L1),
    !,
    replace(B, L2, L3, BR),
    append([A, L3, BR], L4).
replace0(L,_,_,L):-!.

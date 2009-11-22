
len_between(List, El1, El2, Len) :-
	append(L2, [El2 | _], List),
	append(_, [El1 | Sublist], L2),
	length(Sublist, Len).
	
len_and_min(List, Len, Min) :-
	length(List, Len),
	find_min(List, Min).

find_min([H], H) :-!.
find_min([H | T], Min) :-
	find_min(T, TMin),
	(   TMin < H
	->  Min = TMin
	;   Min = H
	).
	
	

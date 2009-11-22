
n([H, H1 | T]) :-
	H1 is H+1,
	n([H1 | T]).
	

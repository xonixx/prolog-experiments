
all([], [], []) :-!.
all([T1 | TL], S, R) :-
	append(S1, SL, S),
	all(TL, SL, RL),
	(   RL = []
	->  R = [T1/S1]
	;  
	(   
	(   member(T1/S1, RL),
	    R = RL
	)
	;
	(   member(T1/_, RL),
	    fail
	)
	;
	(   \+member(T1/_, RL),
	    R = [T1/S1 | RL ]
	)
	)).

solve_all(T, S) :-
	forall(all(T,S,R), (write(R),nl)).
	    

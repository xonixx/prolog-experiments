
combination([H | T], L) :-
	select(H, L, L1),
	combination(T, L1).
combination([], []).
	

solve(Command) :-
	combination(K, [a,b,c,d,e]),
	(   K = Command
	;   K = [_|Command]
	;   K = [_,_|Command]
	;   K = [_,_,_|Command]
	;   K = [_,_,_,_|Command]
	),
	% 1
	member(P, Command), member(P, [a,b,e]),
	\+ (
	  select(P1, Command, C), select(P2, C, _),
	   member(P1, [a,c,d]),
	   member(P2, [a,c,d])
	  ),
	% 2
	(   member(c, Command),
	    \+ member(b, Command)
	->  member(a, Command),
	    \+ member(d, Command)
	;   true
	),
	% 3
	(   member(b, Command),
	    \+ member(c, Command)
	->  member(d, Command),
	    member(e, Command)
	;   true
	),
	% 4
	(   \+ member(a, Command)
	->  member(c, Command),
	    member(e, Command)
	;   true
	).


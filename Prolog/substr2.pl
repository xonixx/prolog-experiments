
substr(S, Str) :-
	concat(_, S1, Str),
	concat(S, _, S1).

substr2(S2, Str) :-
	substr(S2, Str),
	atom_length(S2, 2).
	

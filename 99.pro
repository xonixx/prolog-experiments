:-use_module(library(clpfd)).
s99(R) :-
	R = [
	     [A,B,C],
	     [D,E,F],
	     [G,H,I]
	    ],
	append(R,V),
	
	V ins 1..3,
	
	sum([A,B,C],#=,6),
	sum([D,E,F],#=,6),
	sum([G,H,I],#=,6),
	
	sum([A,D,G],#=,6),
	sum([B,E,H],#=,6),
	sum([C,F,I],#=,6),
	
	sum([A,E,I],#=,6),
	sum([C,E,G],#=,6),
	
	label(V).
	
	

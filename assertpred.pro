
t :-
	assert(my_add(A,B,C):-C is A+B),
	my_add(1,2,R),
	writeln(R).

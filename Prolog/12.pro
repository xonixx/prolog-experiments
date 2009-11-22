

foo0(X,Y):-
	member(X,[1,-1]),
	member(Y,[2,-2]).

foo(X,Y):-
	foo0(X,Y);
	foo0(Y,X).

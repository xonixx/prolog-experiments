

digits(0, []):-!.
digits(N, DD) :-
	N1 is N // 10,
	D1 is N mod 10,
	digits(N1, DD1),
	append(DD1, [D1], DD).
	

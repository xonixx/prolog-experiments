sum([H1,H2 | T],R):-!, H is H1+H2,sum([H | T],R).
sum([H],H).

sum1(From, From, S, S) :-!.
sum1(From, To, S0, S) :-
	S1 is S0 + From,
	From1 is From + 1,
	sum1(From1, To, S1, S).
	
	

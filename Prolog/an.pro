

s0(A,N,S0,N0,S) :-
	N0 < N, !,
	S1 is A * (S0 + 1),
	N1 is N0 + 1,
	s0(A,N,S1,N1,S).
s0(_,_,S,_,S).

s(A,N,Res):-s0(A,N,A,1,Res).
	

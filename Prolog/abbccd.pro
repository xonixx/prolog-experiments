
n(L,N) --> n(L,N,0).

n(_,N,N) --> [], !.
n(L,N,K) --> L, {K1 is K + 1}, n(L, N, K1).

abbccd(N,M) -->
	{M1 is 2*M},
	n("a",N),
	n("b",M1),
	n("c",M1),
	n("d",N).

gen :-
	forall((
	       between(1,4,N),
		between(1,4,M),
		phrase(abbccd(N,M),S),
		string_to_atom(S,A)
	       ),
	       writeln(A)).




solution(Res) -->
	select(A),
	select(B),
	select(C),
	select(D),
	{Res = [A, B, C, D],
	 (   member(boris, Res)	 ->  member(kostja, Res)
	 ;   member(kostja, Res) ->  \+ member(vasilij, Res)
	 ;   member(dima, Res)	 ->  member(jura, Res)
	 ;   member(fedja, Res),
	     (	 member(dima, Res) -> member(grisha, Res)
	     ;
	 )
	}.

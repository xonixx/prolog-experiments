
make_stream(Pred, Head, OpenList) :-
	when(nonvar(OpenList), 
	     (
	     OpenList = [Head|Rest],
	      call(Pred, Head, Next),
	      make_stream(Pred, Next, Rest)
	     )).

ones(Ones) :-
	when(nonvar(Ones),
	     (	 
	     Ones = [1 | Ones1],
		 ones(Ones1)
	     )).

add_streams(S1, S2, S) :-
	when(nonvar(S), 
	     (S1 = [H1 | T1],
	      S2 = [H2 | T2],
	      H is H1 + H2,
	      add_streams(T1,T2,T),
	      S = [H | T]
	     )
	    ).

integers(Ints) :-
	when(nonvar(Ints),
	     (	 
	     Ints = [1 | IntsTail],
		 integers(Ints1),
		 ones(Ones),
		 add_streams(Ints1, Ones, IntsTail)
	     )).
	
	 

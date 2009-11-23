cons_stream(ResStream, Head, Tail, TailProc) :-
	when(nonvar(ResStream), 
	    (ResStream = [Head | Tail],
	     TailProc
	    )).

add_streams(S1, S2, S) :-
 when(nonvar(S), 
      (S1 = [H1 | T1],
       S2 = [H2 | T2],
       H is H1 + H2,
       add_streams(T1,T2,T),
       S = [H | T]
      )).

ones1(Ones) :-
	cons_stream(Ones,
		     1, OtherOnes,
		     (ones1(OtherOnes)	 
		     )).

% yes, infinite list of integers!!!
integers1(Ints) :-
	cons_stream(Ints, 
		    1, IntsTail,
		    (integers1(Ints1),
		     ones1(Ones),
		     add_streams(Ints1, Ones, IntsTail)
		    )).

take(N, Stream, ResultList) :-
	length(ResultList, N),
	append(ResultList, _, Stream).


oneToN(N, Res) :-
	integers1(Ints),
	take(N, Ints, Res).

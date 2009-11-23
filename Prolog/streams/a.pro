ones(Ones) :-
 when(nonvar(Ones),
      (Ones = [1 | Ones1],
       ones(Ones1)
      )).

add_streams(S1, S2, S) :-
 when(nonvar(S), 
      (S1 = [H1 | T1],
       S2 = [H2 | T2],
       H is H1 + H2,
       add_streams(T1,T2,T),
       S = [H | T]
      )).

integers(Ints) :-
 when(nonvar(Ints),
      (Ints = [1 | IntsTail],
       integers(Ints1),
       ones(Ones),
       add_streams(Ints1, Ones, IntsTail)
      )).

cons_stream(ResStream, Head, Tail, TailProc) :-
	when(nonvar(ResStream), 
	    (ResStream = [Head | Tail],
	     TailProc
	    )).

ones1(Ones) :-
	cons_stream(Ones,
		     1, OtherOnes,
		     (ones1(OtherOnes)	 
		     )).

integers1(Ints) :-
	cons_stream(Ints, 
		    1, IntsTail,
		    (integers1(Ints1),
		     ones1(Ones),
		     add_streams(Ints1, Ones, IntsTail)
		    )).
	
fibs(Fibs) :-
	cons_stream(Fibs,
		   0, Tail1,
		   (cons_stream(Tail1,
			       1, Tail2,
				(fibs(Fibs1),
				 fibs(Fibs2), Fibs2 = [_|TailFibs2],
				 add_streams(Fibs1, TailFibs2, Tail2)
				)))).


take(N, Stream, ResultList) :-
	length(ResultList, N),
	append(ResultList, _, Stream).

integers_from(N, Numbers) :-
	cons_stream(Numbers,
		   N, OtherNumbers, 
		   (N1 is N + 1,
		    integers_from(N1, OtherNumbers)
		   )).

not_divisible(H, X) :- X mod H =\= 0.

filter_stream(G, S, S1) :-
	S = [H | T],
	(   call(G, H)
	->  cons_stream(S1,
		       H, T1,
		       (filter_stream(G, T, T1)   
		       ))
	;   filter_stream(G, T, S1)
	).
	

sieve(Numbers, Primes) :-
	Numbers = [H | T],
	cons_stream(Primes,
		    H, OtherPrimes,
		    (filter_stream(not_divisible(H), T, Filtered),
		     sieve(Filtered, OtherPrimes)	
		    )).

primes(Primes) :-
	integers_from(2, IntsFrom2),
	sieve(IntsFrom2, Primes).


test(N) :-
	integers(II),
	nth1(N, II, R1),
	format('~d''s of integers is ~d~n', [N, R1]),
	
	integers1(II1),
	nth1(N, II1, R2),
	format('~d''s of integers1 is ~d~n', [N, R2]),
	
	take(N, II1, L),
	format('first ~d elems of integers1 is ~w~n', [N, L]),
	
	fibs(FF),
	nth0(N, FF, F1),
	format('~d''s of fibs is ~d~n', [N, F1]),
	
	take(N, FF, LF1),
	format('first ~d elems of fibs is ~w~n', [N, LF1]),
	
	primes(PP),
	take(N, PP, PN),
	format('first ~d primes: ~w~n', [N, PN])
	.
	

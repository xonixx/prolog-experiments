cons_stream(ResStream, Head, Tail, TailProc) :-
	when(nonvar(ResStream),
	    (ResStream = [Head | Tail],
	     TailProc
	    )).

take(N, Stream, ResultList) :-
	length(ResultList, N),
	append(ResultList, _, Stream).

take_while(Stream, Pred, ResultList) :-
	Stream = [H | T],
	(   call(Pred, H)
	->  ResultList = [H | Rest],
	    take_while(T, Pred, Rest)
	;   ResultList = []
	).

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

integers_from(N, Numbers) :-
	cons_stream(Numbers,
		   N, OtherNumbers,
		   (N1 is N + 1,
		    integers_from(N1, OtherNumbers)
		   )).

primes(Primes) :-
	integers_from(2, IntsFrom2),
	sieve(IntsFrom2, Primes).


test(N) :-
	primes(PP),
	take(N, PP, PN),
	format('first ~d primes: ~w~n', [N, PN])
	.

less_then(N, X) :- X < N.

test1(N) :-
	primes(PP),
	take_while(PP, less_then(N), PN),
	format('primes less then ~d: ~w~n', [N, PN]).

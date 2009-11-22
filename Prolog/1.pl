
boy(joe).
boy(eddy).
boy(patrick).


factorial(0,1).

factorial(N,F) :-
   N>0,
   N1 is N-1,
   factorial(N1,F1),
   F is N * F1.
   
   
all_less_then(_, [], []).
all_less_then(A, [X | Xs], Y) :- X < A, member(X, Y), all_less_then(A, Xs, Y).
   
qsort([], []).
qsort([_], [_]).
qsort([X | Xs], Y) :-
	!.

do_it(X, Y) :-
	(   X is Y
	->  true
	;   fail
	).
       

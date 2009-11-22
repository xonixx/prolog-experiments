

time(bazz, 5).
time(woody, 10).
time(rex, 20).
time(ham, 25).

% step((L1, L2), (L11, L21), Time)

without(L, Elts, L1) :-
	Elts = [Elt | Oth],
	append(L01, [Elt | L02], L),
	append(L01, L02, L2),
	without(L2, Oth, L22),
	sort(L22, L1),
	!.
without(L, [], L1) :- sort(L, L1).

with(L, L1, L2) :-
	append(L, L1, L11),
	sort(L11, L2).

% without(L, Elts, L1) :-

step((L1, L2), (L11, L21), Time) :-
	member(Toy1, L1),
	member(Toy2, L1),
	Toy1 \= Toy2,
	without(L1, [Toy1, Toy2], L11),
	with(L2, [Toy1, Toy2], L21),
	time(Toy1, Time1),
	time(Toy2, Time2),
	Time is max(Time1, Time2).

stepBack((L11, L21), (L1, L2), Time) :-
	member(Toy, L1),
	time(Toy, Time),
	without(L1, [Toy], L11),
	with(L2, [Toy], L21).

solve(B, A, T) :-
	format('-> ~w, ~w, ~w~n', [B, A, T]), fail.

solve(Before, After, TimeGiven) :-
	TimeGiven > 0,
	(   L1, _) = Before,
	length(L1, Blength),
	(   Blength >= 3
	->  step(Before, S1, Time1),
	    stepBack(S1, S2, Time2),
	    TimeGiven1 is TimeGiven - Time1 - Time2,
	    solve(S2, After, TimeGiven1)
	;   step(Before, After, Time),
	    TimeGiven >= Time
	).

solve :-
	L = [bazz, woody, rex, ham],
	solve((L, []), ([], L), 60).
	
	
	

time(john, 5).
time(pit, 10).
time(tom, 20).
time(pat, 25).
all([john, pit, tom, pat]).

without(With, Without, R) :-
 member(R, With),
 \+ member(R, Without).

allPairs([H | T], H, P2) :-
 member(P2, T).

allPairs([_ | T], P1, P2) :-
 allPairs(T, P1, P2).

step(state(L, yes), state(L1, no), Time) :-
 allPairs(L, T1, T2),
 findall(T, without(L, [T1, T2], T), L1),
 time(T1, Time1),
 time(T2, Time2),
 Time is max(Time1, Time2).

step(state(L, no), state(L1, yes), Time) :-
 all(All),
 without(All, L, T),
 append([T], L, L1),
 time(T, Time).

solve(Inp, Out, TimeGiven, [(Inp->S1/TimeGiven1) | T]) :-
 TimeGiven > 0,
 step(Inp, S1, Time),
 TimeGiven1 is TimeGiven - Time,
 solve(S1, Out, TimeGiven1, T).

solve(state([], _), _, TimeGiven, []) :-
 TimeGiven >= 0.

solve :-
 all(All),
 forall(solve(state(All, yes), _, 60, Solution),
 formatSolution(Solution)).

formatSolution(States) :-
 nl,
 writeln('Solution:'),
 forall(member(state(L1, Has1)->state(L2, Has2)/T, States),
        format('~w ~w -> ~w ~w, time remains:~w~n', [L1, Has1, L2, Has2, T])
 ).



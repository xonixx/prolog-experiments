%% Автор:
%% Дата: 20.05.2007

:- dynamic
 man/1.

  man(pit).
man(joe).

start:-
 retract(man(joe)).
 

isL(X).
isL(X, X):-
 isL(X).
head([H | T], H).
tail([H | T], T).

take([H | T], 0, []):-!.
take([H | T], 1, [H]):-!.
take([H | T], N, [H | ResT]) :-
 is(N1, N-1),
 take(T, N1, ResT).

man(pit).
man(sem).

dog(fred).

man_dog(X) :-
 man(X); dog(X).

fact(N, Res) :-
 (N<2, Res is N);(N1 is N - 1, fact(N1, R), Res is N*R1).


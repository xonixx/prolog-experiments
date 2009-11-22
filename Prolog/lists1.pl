
len([], 0).
len([_ | Tail], L) :-
        len(Tail, Lt),
        is(L, Lt + 1).
        
reverse1([], []).
reverse1([H | T], Z):-
        reverse1(T, T1),
        append(T1, [H], Z).
        
palindrom(L):-
  reverse1(L, L).

make_set1([], []).
make_set1([_], [_]).
make_set1([X | Y], R):-
  member(X, R),
  make_set1(Y, R).
make_set1([X | Y], R):-
  R1 is [X | R],
  make_set1(Y, R1).

is_set1([]).
is_set1([X | Xs]):-
  not(member(X, Xs)),
  is_set1(Xs).

isL(L, R):-
  append([], L, R).

setWithEl(El, L1, L2):-
 member(El, L1),
 isL(L2, L1),!.
setWithEl(El, L1, L2):-
 isL([El | L1], L2).
/*
set([], []).
set([X, Xs], Res1, Res):-
 isL(),
 setWithEl(X, R1, Res).
*/

set1([], []).
set1([_], [_]).
set1([X | Xs], [Y | Ys]):-
 member(X, Xs),!,
 is(Y, -1),
 set1(Xs, Ys).
 
set1([X | Xs], [Y | Ys]):-
 not(member(X, Xs)),!,
 is(Y, X),
 set1(Xs, Ys).
 
set2([], []).
set2([H | T1], [H | T2]):-
 delete(T1, H, Tres),
 set2(Tres, T2).
 

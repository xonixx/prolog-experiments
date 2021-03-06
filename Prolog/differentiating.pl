
:- use_module(xlists).

isvar(x).
isvar(y).
isvar(z).

isfunc(f).
isfunc(g).
isfunc(h).

concatAll([], '').
concatAll([H | T], Res) :-
 concatAll(T, R),
 concat(H, R, Res).
 
%%
%% displ
%%

displ(A, A) :-
 number(A);
 atom(A).

displ(add(A, 0), Res) :-
 displ(A, Res), !.
 
displ(add(0, B), Res) :-
 displ(B, Res), !.
 
displ(add(A, B), Res) :-
 number(A),
 number(B),
 Res is A+B,
 !.

displ(add(A, B), Res) :-
 displ(A, R1),
 displ(B, R2),
 (number(R1), number(R2) -> displ(add(R1, R2), Res);
                         concatAll([R1, ' + ', R2], Res)).

displ(mul(_, 0), 0) :-!.
displ(mul(0, _), 0) :-!.
displ(mul(A, B), Res) :-
 number(A),
 number(B),
 Res is A*B,
 !.

displ(mul(A, B), Res) :-
 displ(A, R1),
 displ(B, R2),
 concatAll(['(', R1, ' * ', R2, ')'], Res).
 
displ(A+B, Res) :-
 displ(add(A, B), Res).

displ(A*B, Res) :-
 displ(mul(A, B), Res).
 
displ(diff(V), Res) :-
 isvar(V),
 concatAll([V, ''], Res).

%%
%% diff
%%

diff(A, Res) :-
 isvar(A),!,
 Res = diff(A).

diff(A, 0) :-
 number(A);
 atom(A).
 
diff(mul(A, B), Res) :-
 diff(A, R1),
 diff(B, R2),
 Res = add(mul(R1, B), mul(A, R2)).

diff(add(A, B), Res) :-
 diff(A, R1),
 diff(B, R2),
 Res = add(R1, R2).

diff(A*B, Res) :-
 diff(mul(A, B), Res), !.
 
diff(A+B, Res) :-
 diff(add(A, B), Res), !.
 
diff(A-B, Res) :-
 diff(add(A, -B), Res), !.
 
%%
%% simplify/2
%%
 
numbers([]).
numbers([H | T]) :-
 number(H),
 numbers(T).

connect_plus([A], A) :-!.
connect_plus([H | T], Res) :-
 connect_plus(T, R1),
 Res = H+R1.

simplify(add(A, B), Res) :-
 (numbers([A, B]) -> Res is A + B);
 (
 simplify(A, R1),
 simplify(B, R2),
 (numbers([R1, R2]) -> Res is R1 + R2);
  (
  Res = add(R1, R2)
  )
 ).
 
simplify(add([A]), A) :-!.
simplify(add(List), Res) :-
 xlists:filter(List, number, Nums, NotNums),
 sumlist(Nums, Sum),
 connect_plus([Sum | NotNums], Res),
 !.
 
simplify(add(A, add(B, C)), Res) :-
 simplify(add([A, B, C]), Res).
 
simplify(add(add(A, B), C), Res) :-
 simplify(add([A, B, C]), Res).
 
simplify(mul(0, _), 0) :-!.
simplify(mul(_, 0), 0) :-!.
simplify(mul(A, B), Res) :-
 (numbers([A, B]) -> Res is A * B);
 (
 simplify(A, R1),
 simplify(B, R2),
 (numbers([R1, R2]) -> Res is R1 * R2);
  (
  Res = mul(R1, R2)
  )
 ).
 
simplify(A+B, Res) :-
 simplify(add(A, B), Res), !.
 
simplify(A*B, Res) :-
 simplify(mul(A, B), Res), !.
 
% if nothing applied
simplify(A, A).

ferz([]).
ferz( [X/Y | Others]) :-
    ferz( Others),
    L = [1,2,3,4,5,6,7,8],
    member(Y,L),
    member(X,L),
    move(X/Y, Others).
move(_,[]).
move(X/Y, [X1/Y1 | Others]) :-
    (goriz(X/Y,X1/Y1);diag(X/Y,X1/Y1);vert(X/Y,X1/Y1)),
    move( X1/Y1, Others).
goriz(X/Y,X1/Y1):-
    X =\= X1,
    Y=:=Y1.
vert(X/Y,X1/Y1):-
    Y =\= Y1,
    X=:=X1.
diag(X/Y,X1/Y1):-
    Y =\= Y1,X =\= X1,
    (Y1-Y =:= X1-X; Y1-Y =:= X-X1).
member( Item, [Item | Rest]).
member( Item, [First | Rest]):-
    member(Item, Rest).
add(X,L,L) :- member(X,L),!.
add(X,L,[X | L]).
%template( [1/1,X2/Y2,X3/Y3,X4/Y4,X5/Y5,X6/Y6,X7/Y7,X8/Y8,X9/Y9,X10/Y10,X11/Y11,X12/Y12,X13/Y13,X14/Y14,1/1] ).
template( [1/6,X2/Y2,X3/Y3,X4/Y4,X5/Y5] ).
writelist([]).
writelist([X | L]):-
    write('('),write(X),write(')'),
    writelist(L).
addlist([],L):-!.
addlist([],[]):-!.
addlist([X1/Y1 | Others],L2):-
    add(X1/Y1,L2,L),
    addlist(Others,L).
ecv([],[]).
ecv([X1/Y1| End1],[X2/Y2 | End2]):-
    X1/Y1 = X2/Y2,
    ecv(End1,End2).
variantu(S):-template(S),ferz(S).
koordinatu([],Prohod).
koordinatu( [X/Y | Others],Prohod) :-
    koordinatu(Others,Prohod),
    rashirim(X/Y,Others,Prohod).
%
concat([],[],[]):-!.
concat([],L2,L2).
concat(L1,[],L1).
concat([X|L1],L2,[X|L3]):-concat(L1,L2,L3).
%

rashirim(_,[],Prohod):-!.
rashirim(X1/Y1, [X2/Y2 | Others],Prohod) :-
    ((X2>X1, Y1=:=Y2,bagof(X/Y,uslovie1(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (X1>X2, Y1=:=Y2,bagof(X/Y,uslovie2(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (Y2>Y1, X1=:=X2,bagof(X/Y,uslovie3(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (Y1>Y2, X1=:=X2,bagof(X/Y,uslovie4(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (X1>X2, Y1>Y2,bagof(X/Y,uslovie5(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (X1>X2, Y2>Y1,bagof(X/Y,uslovie6(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (X2>X1, Y1>Y2,bagof(X/Y,uslovie7(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod));
    (X2>X1, Y1<Y2,bagof(X/Y,uslovie8(X1/Y1,X2/Y2,X/Y),List),addlist(List,Prohod))),
    write(Prohod), nl,
    rashirim( X2/Y2, Others,Prohod).
uslovie1(X1/Y1,X2/Y2,X/Y):-
    between(X1,X2,X),
    Y is Y1.
uslovie2(X1/Y1,X2/Y2,X/Y):-
    between(X2,X1,X),
    Y is Y1.
uslovie3(X1/Y1,X2/Y2,X/Y):-
    between(Y1,Y2,Y),
    X is X1.
uslovie4(X1/Y1,X2/Y2,X/Y):-
    between(Y2,Y1,Y),
    X is X1.
uslovie5(X1/Y1,X2/Y2,X/Y):-
    between(X2,X1,X),
    between(Y2,Y1,Y),
    X =:= Y.
uslovie6(X1/Y1,X2/Y2,X/Y):-
    between(X2,X1,X),
    between(Y1,Y2,Y),
    X1-X =:= Y-Y1.
uslovie7(X1/Y1,X2/Y2,X/Y):-
    between(X1,X2,X),
    between(Y2,Y1,Y),
    X-X1 =:= Y1-Y.
uslovie8(X1/Y1,X2/Y2,X/Y):-
    between(X1,X2,X),
    between(Y1,Y2,Y),
    X-X1 =:= Y-Y1.
isitreal([]).
isitreal([X/Y | End]):-
    integer(X),
    integer(Y),
    isitreal(End).
solve(S,D):-variantu(S),koordinatu(S,D).
otv(S):-solve(S,D),writelist(D),nl,
        memberchk(1/3,D),
        memberchk(4/3,D),
        memberchk(5/4,D),
        memberchk(6/5,D),
        memberchk(7/6,D),
        memberchk(7/2,D).
otv1(S):-solve(S,D),isitreal(D),length(D,7).

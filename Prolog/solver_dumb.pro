%FAJCKVABBKQ * BVKCJFQAAN = CFKFAKQKAAACVDNKCAKK
%

:-use_module(library(clpfd)).

n([H],  H):-!.
n([H | T], R) :-
    n(T, Rt),
    length(T, L),
    Ten is 10^L,
    R = H * Ten + Rt.

mul(L1, L2, L, DoMod) :-
    length(L1, Len),
    n(L1, N1),
    n(L2, N2),
    n(L, N),
    (   DoMod
    -> (   N1 * N2) mod 10^Len #= N
    ;   N1 * N2 #= N
    ).

solve([F,A,J,C,K,V,A,B,B,K,Q]
      * [B,V,K,C,J,F,Q,A,A,N]
      = [C,F,K,F,A,K,Q,K,A,A,A,C,V,D,N,K,C,A,K,K]) :-

    [Q,N,K] ins 0..9,
    all_different([Q,N,K]),
    mul([Q],[N],[K],true),

    [A] ins 0..9,
    all_different([Q,N,K,A]),
    mul([K,Q], [A,N], [K,K], true),

    [B] ins 0..9,
    all_different([Q,N,K,A,B]),
    mul([B,K,Q], [A,A,N], [A,K,K], true),

    label([Q,N,K,A,B]),

    AllVars = [A,C,B,D,F,K,J,N,Q,V],
    AllVars ins 0..9,
    all_different(AllVars),

    mul([F,A,J,C,K,V,A,B,B,K,Q]
      , [B,V,K,C,J,F,Q,A,A,N]
      , [C,F,K,F,A,K,Q,K,A,A,A,C,V,D,N,K,C,A,K,K], false),

    label(AllVars)
    .

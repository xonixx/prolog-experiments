
people([]).
people([X/Y | Others]) :-
  people(Others),
  member(X, [leonid, sergey, nikolay, oleg, petr]),
  member(Y, [antonov, borisov, vasilev, drozdov, ivanov]),
  \+ member(X/_, Others),
  \+ member(_/Y, Others).
  

knows(N1/F1, N2/F2) :-
  N1\=N2,
  F1\=F2.
  
result([leonid/L, sergey/S, nikolay/N, oleg/O, petr/P]) :-
  people([leonid/L, sergey/S, nikolay/N, oleg/O, petr/P]).

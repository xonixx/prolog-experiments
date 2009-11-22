

add(A, B, C) :-
 (number(A), number(B), C is A + B);
 (number(A), number(C), B is C - A);
 (number(B), number(C), A is C - B).


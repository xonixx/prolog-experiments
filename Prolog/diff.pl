

diff(mult(a, b)) :-
 diff( add( mult(diff(a), b), mult(a, diff(b)) ) ).
 
b1(bbb(c), R):-
 is(bbb(c), R).

a1(aaa(a,b), R):-
   b1(bbb(a, b), R).

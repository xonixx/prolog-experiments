
from09(X, Without) :- 
	member(X, [0,1,2,3,4,5,6,7,8,9]), 
	\+ member(X, Without).

solution([
	  [D, O, N, A, L, D],
	  [G, E, R, A, L, D],
	  [R, O, B, E, R, T]
	 ]) :-
	
	from09(G, []), 
	from09(E, [G]), 
	from09(R, [G, E]),
	from09(O, [G, E, R]), 
	from09(N, [G, E, R, O]), 
	from09(A, [G, E, R, O, N]), 
	from09(L, [G, E, R, O, N, A]), 
	from09(D, [G, E, R, O, N, A, L]),

	T is (D + D) mod 10, T1 is (D + D) // 10,
	R is (T1 + L + L) mod 10, R1 is (T1 + L + L) // 10,
	E is (R1 + A + A) mod 10, E1 is (R1 + A + A) // 10,
	B is (E1 + N + R) mod 10, B1 is (E1 + N + R) // 10,
	O is (B1 + O + E) mod 10, O1 is (B1 + O + E) // 10,
	R is (O1 + D + G) mod 10, 0 is (O1 + D + G) // 10.

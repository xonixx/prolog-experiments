% example:
% 17 ?- max([1,2,3,2,7,2,3], R).
%
% R = 7
% 18 ?- max([1,2,3,2,-7,2,3], R).
%
% R = 3

max([A], A) :-!.
max([H | T], H) :-
	max(T, Tmax),
	H > Tmax,
	!.

max([_ | T], Tmax) :-
	max(T, Tmax).

%%%

max_even([_], _) :- 
	write('Need more or equal 2 elements!'),
	fail, !.
max_even([_, B], B) :-!.
max_even([_, B, _], B) :-!.
max_even([_, B | T], Tmax) :-
	max_even(T, Tmax),
	Tmax > B, !.
max_even([_, B | _], B) :-!.

%%%

remove_even(List, ResList) :-
	re(List, ResList, 1).

% example:
% 29 ?- remove_even([1,2,3,4,5,6,67,8,9], R).
% R = [1, 3, 5, 67, 9]

re([], [], _) :-!.
re([_ | T], T1, 0) :-
	re(T, T1, 1),
	!.

re([H | T], [H | T1], 1) :-
	re(T, T1, 0).


	

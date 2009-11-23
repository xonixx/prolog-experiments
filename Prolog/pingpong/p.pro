
ping :-
	freeze(X, pong),
	format('ping~n',[]),
	X = 1.
pong :-
	freeze(X, ping),
	format('pong~n',[]),
	X = 1.

ping1 :-
	format('ping~n',[]),
	pong1.
pong1 :-
	format('pong~n',[]),
	ping1.

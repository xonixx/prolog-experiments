

id(1).
id(3).
id(2).
id(10).
id(7).

setMax(N) :-
	retract(max(_)),
	assert(max(N)).

setMax(N) :-
	assert(max(N)).

m(Max) :-
	setMax(0),
	m1,
	max(Max).

m1 :-
	id(Id),
	max(C),
	Id > C,
	setMax(Id),
	fail.
m1.


max_id(CurrId) :-
	id(Id),
	Id > CurrId,
	max_id(Id).

max_id(CurrId) :-
	!, write(CurrId).
	

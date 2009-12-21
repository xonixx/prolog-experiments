
unic([H | T], [H | R1]) :- 
	subtract(T, [H], T1),
	unic(T1, R1).
unic([], []):-!.

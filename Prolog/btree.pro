
walk(A, [A]) :- atomic(A), !.
walk(t(Left,Root,Right), Path) :-
	walk(Right,RightPath),
	walk(Left, LeftPath),
	append([RightPath, [Root], LeftPath], Path).


dimentions(Matr, M, N) :-
	length(Matr, M),
	[F | _] = Matr,
	length(F, N).

validToMultiply(M1, M2) :-
	dimentions(M1, _, N),
	dimentions(M2, N, _).

isRow(Row) :-
	dimentions(Row, 1, _), !.
%isRow(Row) :-
%	write(Row), write(' is not a row!'), nl,
%	throw(notARow(Row)).

isCol(Col) :-
	dimentions(Col, _, 1), !.
%isCol(Col) :-
%	write(Col), write(' is not a col!'), nl,
%	throw(notACol(Col)).

getXY(Matr, Rn, Cn, El) :-
	nth1(Rn, Matr, Row),
	nth1(Cn, Row, El).

firstRow(Matr, [First], LastMatr) :-
	[First | LastMatr] = Matr.

firstCol(Matr, First, LastMatr) :-
	firstRow(Matr, Fr, L1),
	[[H | T]] = Fr,
	(   L1 \= [] ->
	firstCol(L1, Fc, L2),
	append([[H]], Fc, First),
	append([T], L2, LastMatr)
	;
	First = [[H]],
	LastMatr = [T]    
	).

mulRowCol([[A]], [[B]], C) :-
	C is A * B, !.
mulRowCol(Row, Col, Res) :-
	isRow(Row),
	isCol(Col),
	getXY(Row, 1, 1, R1),
	getXY(Col, 1, 1, C1),
	[[_ | PreRow1]] = Row,
	Row1 = [PreRow1],
	[_ | Col1] = Col,
	mulRowCol(Row1, Col1, Res1),
	Res is Res1 + R1 * C1.

mulRowMatr(Row, Matr, [Res]) :-
	isCol(Matr),
	mulRowCol(Row, Matr, Res), !.

mulRowMatr(Row, Matr, [R | Rn]) :-
	firstCol(Matr, Col1, ColN),
	mulRowCol(Row, Col1, R),
	mulRowMatr(Row, ColN, Rn).

mulMatrMatr([], _, []):-!.
mulMatrMatr(M1, M2, Mres) :-
	firstRow(M1, F1, M11),
	mulRowMatr(F1, M2, RowRes),
	mulMatrMatr(M11, M2, MRes),
	Mres = [RowRes | MRes].
	
	
	

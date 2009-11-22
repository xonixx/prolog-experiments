
p(3,7). p(5,3). p(1,3). p(6,8). p(7,9). p(6,2). p(4,3). p(7,3).
p(3,3). p(5,5). p(4,5). p(2,7). p(6,1). p(1,2). p(9,3). p(4,1).
p(1,4). p(5,6). p(8,8). p(2,3). p(3,5). p(4,9). p(6,7). p(1,4).

rect(X1,Y1,X2,Y2, IsSquare, Area) :- 
	p(X1,Y1),
	p(X2,Y1),
	X2>X1,
	p(X1,Y2),
	Y2>Y1,
	p(X2,Y2),
	Area is (X2 - X1) * (Y2 - Y1),
	(   X2 - X1 =:= Y2 - Y1
	->  IsSquare = yes
	;   IsSquare = no
	).

reportRect(rect(X1, Y1, X2, Y2, IsSquare, Area)) :-
	member(IsSquare/Word, [yes/'Квадрат', no/'Прямоугольник']),
	format('~w: (~w,~w) (~w,~w), площадь: ~w~n', 
	       [Word, X1, Y1, X2, Y2, Area]).

maxArea([R1, R2 | Rects], R) :-
	R1 = rect(_,_,_,_,_,A1),
	R2 = rect(_,_,_,_,_,A2),
	(   A1 > A2
	->  R0 = R1
	;   R0 = R2
	),
	maxArea([R0 | Rects], R).
maxArea([R],R):-!.
	
reportMaxArea(Rects) :-
	maxArea(Rects, R),
	format('~nМаксимальная площадь у фигуры:~n',[]),
	reportRect(R).
	

go :-
	findall(R, 
		(   R=rect(_,_,_,_,_,_),
		    R
		   ), Rects),
	forall(member(R, Rects), reportRect(R)),
	reportMaxArea(Rects).

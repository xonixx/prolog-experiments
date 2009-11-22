
%; "*"-работник движется вдоль строки, и выполняет "работу" меняет 0 -> 1, 1 -> 0
%*0 > 1*
%*1 > 0*

%; уничтожение "*"-работника, алгоритм завершается.
%* > .

%; ставим в самом начале звездочку-"работника" (замена пустой подстроки на *)
%> *

:- dynamic [input/1, step/3, print/0].

%input('1000111011').
%print.
%step('*0', '1*', go).
%step('*1', '0*', go).
%step('*', '', stop).
%step('', '*', go).

input(_).
step(_,_,_).

step1(Inp, Out, GoStop) :-
	step(A, B, GoStop),
	concat(I1, I2, Inp),
	concat(I3, A, I1), !,
	concat(I3, B, I4),
	concat(I4, I2, Out).

wr([]):-!.
wr([H | T]):-
	write(H),wr(T).

pr(A):-wr(A),nl.

process(Inp, Out) :-
	!,
	step1(Inp, Out1, GoStop),
	pr([Inp,-->,Out1, ' ', GoStop]),
	(   GoStop=go
	->  process(Out1, Out)
	;   GoStop=stop
	->  Out=Out1	
	;   fail
	).

parseStep(A->B/stop):-
	assert(step(A, B, stop)), !.
parseStep(A->B) :-
	assert(step(A, B, go)).

parseAlg(Inp, Alg) :-
	retractall(input(_)),
	retractall(step(_,_,_)),
	assert(input(Inp)),
	forall(member(Step,Alg),parseStep(Step)).

processAlg(Inp, Alg, Out):-
	parseAlg(Inp, Alg),
	process(Inp, Out).

solveInverse(Inp, Out) :-
	processAlg(Inp,
		   [
		    '*0' -> '1*',
		    '*1' -> '0*',
		    '*' -> '' / stop,
		    '' -> '*'
		   ],
		   Out
		  ).
checkSolveInverse :-
	nl,
	pr(['Checking solve inverse:']),
	solveInverse('1000111011', '0111000100').

solveDouble(A, B):-
	processAlg(
		  A,
		   [
		   
		    '0[0]'->'[0]0',
		    '0[1]'->'[0]1',

		    '1[0]'->'[0]2',
		    '1[1]'->'[0]3',

		    '2[0]'->'[0]4',
		    '2[1]'->'[0]5',

		    '3[0]'->'[0]6',
		    '3[1]'->'[0]7',

		    '4[0]'->'[0]8',
		    '4[1]'->'[0]9',

		    '5[0]'->'[1]0',
		    '5[1]'->'[1]1',

		    '6[0]'->'[1]2',
		    '6[1]'->'[1]3',

		    '7[0]'->'[1]4',
		    '7[1]'->'[1]5',

		    '8[0]'->'[1]6',
		    '8[1]'->'[1]7',

		    '9[0]'->'[1]8',
		    '9[1]'->'[1]9',

		    '[0]'->'' / stop,
		    '[1]'->'1' / stop,
		    '*0'->'0*',
		    '*1'->'1*',
		    '*2'->'2*',
		    '*3'->'3*',
		    '*4'->'4*',
		    '*5'->'5*',
		    '*6'->'6*',
		    '*7'->'7*',
		    '*8'->'8*',
		    '*9'->'9*',

		    '*'->'[0]',

		    ''->'*'
		   ],
		   B
		  ).
checkSolveDouble :-
	nl,
	pr(['Checking solve double:']),
	solveDouble('948', '1896').

checkAll :-
	checkSolveInverse,
	checkSolveDouble.










































:- dynamic [input/1, step/3, print/0, dbg/0, dbg/1].

input(_).
step(_,_,_).

dbg.
dbg(Smth):-
	(   dbg
	->  pr(['DEBUG: ' | Smth])
	;   true
	).

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

expandTerm(A-A, []):-!.
expandTerm(A-B, C) :-
	integer(A), 
	integer(B),
	A1 is A+1,
	expandTerm(A1-B, C1),
	C = [A | C1], !.
expandTerm(A, [A]).
	

expandRange([], []) :-!.
expandRange([H | T], R) :-
	expandTerm(H, HExpanded),
	expandRange(T, TExpanded),
	append(HExpanded, TExpanded, R).

calcAll([], []) :-!.
calcAll([{H} | T], [H1 | T1]) :-
	(   atom(H)
	->  H1 = H
	;   H1 is H
	),
	calcAll(T, T1),
	!.
calcAll([H | T], [H | T1]) :-
	calcAll(T, T1).

parseStep(L0->L1/GoStop) :-
	is_list(L0),
	dbg(['Parsing list step: ', L0->L1/GoStop]),
	(   append(P0, [T=Range | P1], L0)
	->  !,
	    expandRange(Range, RangeExpanded),
	    forall(member(C, RangeExpanded),
		   (
		   T=C,
		    append(P0, [C | P1], L00),
		    parseStep(L00->L1)
		   ))
	;   calcAll(L1, L1Calc),
	    concat_atom(L0, L0Atom),
	    concat_atom(L1Calc, L1Atom),
	    parseStep(L0Atom -> L1Atom/GoStop)
	),
	!.

parseStep(A->B/GoStop) :-
	dbg(['Parse simple step: ', A->B/GoStop]),
	assert(step(A, B, GoStop)), !.
parseStep(A->B) :-
	parseStep(A->B/go).

parseAlg(Alg) :-
	retractall(step(_,_,_)),
	forall(member(Step,Alg),parseStep(Step)).

processAlg(Inp, Alg, Out):-
	parseAlg(Alg),
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

solveAdd(Inp, Out) :-
	concat_atom([^,Inp,$],Inp1),
	processAlg(Inp1,
		  [
['^@[', N=[0,1], ']'] -> ['^', {N}],

'9@[1]' -> '@[1]0',

'^%%%0' -> '^%%%',
'^%%%' -> '^'/stop,

'*%%%' -> '%%%',
[ N=[0-10], '%%%' ] -> ['%%%', {N}],

'+#$' -> '%%%$',

[ N=[0-10], '@[1]' ] -> [{N+1}],
[ N=[0-10], '@[0]' ] -> [{N}],
[ N1=[0-10], '*[', N2=[1-10], ']' ] -> [ '@[', { (N1+N2)//10 }, ']*', { (N1 + N2) mod 10 } ],

[ '*%[', N=[0-10], ']' ] -> [ '*[', {N}, ']' ],

[ N1=[0-10,'+'], '%[', N2=[0-10], ']'] -> ['%[', {N2}, ']', {N1} ],

[ #, N=[0-10,+] ] -> [{N}, #],

[ N=[0-10], '#$' ] -> [ '%[', {N}, ']$' ],

'*' -> '*#',
'+' -> '*+'
		  ],
		  Out1),
	atom_concat(^, Out2, Out1),
	atom_concat(Out, $, Out2).



checkSolveAdd :-
	nl,
	pr(['Checking add 7+49494:']),
	solveAdd('7+49494', '49501').

checkAll :-
	retractall(dbg),
	checkSolveInverse,
	checkSolveDouble,
	checkSolveAdd.



son(son1).
son(son2).

daughter(daughter1).
daughter(daughter2).


adult(father).
adult(mother).
adult(police).


notsafe_(criminal, X) :- X \= police.
notsafe_(mother, Y) :- son(Y).
notsafe_(father, Y) :- daughter(Y).

notsafe(X, Y) :- notsafe_(X, Y); notsafe_(Y, X).

safe(X, Y) :- \+ notsafe(X, Y).

safebridge([X, Y]) :- (adult(X); adult(Y)), safe(X, Y).
safebridge([X]) :- adult(X).


all([
     son1, son2, father, 
     daughter1, daughter2, mother,
     criminal, police
    ]).


allsafe(L) :-
	forall(member(H, L),
	      (	  adult(H)
	      ;	  son(H), 
		  (   member(mother, L),
		      member(father, L)
		  ;   true
		  )
	      ;	  daughter(H),
		  (   member(father, L),
		      member(mother, L)
		  ;   true
		  )
	      ;	  H = criminal, member(police, L)	      
	      )).
allsafe([_]).
allsafe([]).

:- dynamic [inprocess/1].
	
step_(state(Left1, left), state(Left2, right)) :-
	(   member(A, Left1),
	    member(B, Left1), A \= B,
	    OnBridge = [A, B]
	;   member(A, Left1),
	    OnBridge = [A]
	),
	
	safebridge(OnBridge),
	
	subtract(Left1, OnBridge, Left2),
	allsafe(Left2),
	
	all(All),
	subtract(All, Left2, Right),
	allsafe(Right),
	
	retract(inprocess(Left1))
	.

step(state(Left1, left), state(Left2, right)) :-
	step_(state(Left1, left), state(Left2, right)).

step(state(Left1, right), state(Left2, left)) :-
	all(All),
	subtract(All, Left1, Right1),
	step_(state(Right1, left), state(Right2, right)),
	subtract(All, Right2, Left2).
/*
same(L1, L2) :-
	sort(L1, L),
	sort(L2, L).
*/
solve(Inp, Outp, [step(Inp, S1) | Steps]) :-
	Inp = state(L, _),
	sort(L, Ls),
	step(Inp, S1), 
	
	%S1 = state(L1, _),
	%(   \+ same(L1, L2) % do not go back
	%->  true
	%;   format('? ~w ~w~n', [L1, L2]), fail
	%),
	solve(S1, Outp, Steps).

solve :-
	all(All),
	solve(state(All, left), state([], _), state([], _), _).















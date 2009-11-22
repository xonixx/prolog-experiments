

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
		  (   member(mother, L)
		  ->  member(father, L)
		  ;   true
		  )
	      ;	  daughter(H),
		  (   member(father, L)
		  ->  member(mother, L)
		  ;   true
		  )
	      ;	  H = criminal, member(police, L)	      
	      )).
allsafe([_]).
allsafe([]).

	
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
	allsafe(Right).

step(state(Left1, left), state(Left2, right)) :-
	step_(state(Left1, left), state(Left2, right)).

step(state(Left1, right), state(Left2, left)) :-
	all(All),
	subtract(All, Left1, Right1),
	step_(state(Right1, left), state(Right2, right)),
	subtract(All, Right2, Left2).

:- dynamic [inprocess/1].

preventLoop(state(L, Lr)) :-
	sort(L, Ls),
	Inp1 = state(Ls, Lr),
	
	\+ inprocess(Inp1),
	assert(inprocess(Inp1)).	

solve(Inp, Outp, [step(Inp, S1) | Steps]) :-

	preventLoop(Inp),
	
	step(Inp, S1), 
	
	(   S1 = Outp
	->  Steps = []
	;   solve(S1, Outp, Steps)
	).

solve :-
	retractall(inprocess(_)),
	all(All),
	forall(solve(state(All, left), state([], _), Steps),
	      (	  format('~nSolution:~n'),	  
		  forall(member(Step, Steps), 
			 printStep(Step)
			)
	      )).

printStep(step(state(L1, Pos1), state(L2, _))) :-
	(   Pos1 = left
	->  subtract(L1, L2, Moved),  
	    format('~w -> left: ~w~n', [Moved, L2])
	;   Pos1 = right
	->  subtract(L2, L1, Moved),
	    format('~w <- left: ~w~n', [Moved, L2])
	).















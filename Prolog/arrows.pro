
solution(Begin, End, PrevSteps, [Step | Steps]) :-
	Step = step(Begin, State1),
	Step,
	forall(member(step(S, _), PrevSteps),
	       State1 \= S
	      ), % prevent loops
	(   State1 == End
	->  Steps = []
	;   solution(State1, End, [Step | PrevSteps], Steps)
	).

rev(->,<-).
rev(<-,->).

step([X,Y|T], [XX,YY|T]) :- rev(X,XX), rev(Y, YY).
step([A,X,Y|T], [A,XX,YY|T]) :- rev(X,XX), rev(Y, YY).
step([A,B,X,Y|T], [A,B,XX,YY|T]) :- rev(X,XX), rev(Y, YY).
step([A,B,C,X,Y|T], [A,B,C,XX,YY|T]) :- rev(X,XX), rev(Y, YY).
step([A,B,C,D,X,Y], [A,B,C,D,XX,YY]) :- rev(X,XX), rev(Y, YY).


run :-
	solution([->,<-,->,<-,->,<-], [<-,<-,<-,->,->,->],[],Steps),
	!,
	forall(member(Step,Steps),writeln(Step)).


:- dynamic [eq/2].
:- use_module(pr).

errorRun(S) :-
	wr('Error: '), pr(S).

eval(B, _) :-
	dbg(['eval: ', B]), fail.

eval(B, B) :-
	number(B), !.

eval(B, B1) :-
	atom(B),
	eq(B, B1), !.
	
eval(B, B1) :-
	B =.. [Op, A1, A2],
	eval(A1, A11),
	eval(A2, A22),
	B0 =.. [Op, A11, A22],
	(   member(Op, [+, -, *, /, //, mod])
	->  B1 is B0
	;   member(Op, [<, >, =<, >=, =:=, =\=])
	->  (
		B0
		->  B1=1
		;   B1=0
	    )
	),
	!.
% and
eval((A1, A2), B1) :-
	eval(A1, A1Res),
	(   A1Res=0
	->  B1=0
	;   eval(A2, B1)
	),
	!.

% or
eval((A1; A2), B1) :-
	eval(A1, A1Res),
	(   A1Res=0
	->  eval(A2, B1)
	;   B1=1
	),
	!.

eval(B, _) :-
	throw(interp_error(eval, B)),
	!.
	
runStmt(A) :-
	dbg(['runStmt: ', A]), fail.

runStmt({}) :-!.
runStmt({Code}) :-
	run(Code),
	!.

runStmt(A=B) :-
	(   atom(A)
	->  eval(B, B1),
	    retractall(eq(A, _)),
	    assert(eq(A, B1))
	;   errorRun([A, ' is not var'])),
	!.

runStmt(write(A)) :-
	(   eval(A, A1)
	->  write(A1)
	;   write(A)
	),
	!.

runStmt(for(Start, Cond, Step, Code)) :-
	run(Start),
	repeat,
	(      
	    eval(Cond, CondRes),
	    CondRes=1
	->  run(Code),
	    run(Step),
	    fail
	;   true, !
	),
	!.

runStmt(while(Cond, Code)) :-
	runStmt(for({}, Cond, {}, Code)),
	!.

runStmt(if(Cond, Code)) :-
	runStmt(if(Cond, Code, {})),
	!.

runStmt(if(Cond, Code, CodeElse)) :-
	eval(Cond, CondRes),
	(   CondRes=1
	->  run(Code)
	;   run(CodeElse)
	),
	!.

runStmt(Stmt) :-
	throw(interp_error(runStmt, Stmt)),
	!.

runSafe(Code) :-
	catch(run(Code),
	      interp_error(Meth, Args),
	      (	  errorRun(['Caught error while executing: "', Meth, '" with args: ', Args])
	      )),
	!.

run(Code) :-
	dbg(['run: ', Code]), fail.

run(Code) :-
	( Stmt, OtherStmts ) = Code,
	runStmt(Stmt),
	run(OtherStmts), 
	!.

run(Stmt) :-
	runStmt(Stmt),
	!.

testEul1 :-
	runSafe((
	    n=0,
	    sum=0,
	    while(n<1000, 
		  {
		   if((n mod 5 =:= 0; n mod 3 =:= 0),
		     {
		      sum = sum + n
		     }),
		   n = n + 1
		  }),
	     write(sum)
	    )).

test3 :-
	run((
	    if(5<2, 
	       {
		write('true')
	       },
	       {
		write('false')
	       })
	    )).

test2 :-
	run((
	   function(add, (a, b), {
			    return(a+b)
			    }),
	     write(add(2, 5))
	    )).

test1 :-
	run(
	   (
	   a=10,
	    b=20+10/2,
	    c=a+b,
	    write(c)
	       
	   )
	   ).

test :-
	run(
	   (   
	   sum = 0,
	   for(i=0, i<10, i=i+1,  
	   {
		sum = sum + i		    
	   }),
	   write('sum='),
	   write(sum)
	   )
	   ).























:- style_check(-atom). % long strings embedded
:- dynamic [eq/3, % level, name, value
	   funcs/2, % fname, fargs, fcode
	   level/1 % stack depth
	   ].
:- use_module(pr).

level(0).

errorRun(S) :-
	wr('Error: '), pr(S).

set(Functor) :-
	Functor =.. [PredName, PredKey, _],
	FunctorDel =.. [PredName, PredKey, _],
	retractall(FunctorDel),
	assert(Functor).

%%
%
%	setFuncVars(FargNames, Fargs)
%	
%	
setFuncVars(FargNames, Fargs) :-
	dbg(['setFuncVars: ', FargNames,' ', Fargs]), fail.

setFuncVars((Name, NameOth), [Val | ValOth]) :-
	setFuncVars(Name, [Val]),
	setFuncVars(NameOth, ValOth),
	!.

setFuncVars(Name, [Val]) :-
	level(N),
	eval(Val, Val1),
	retractall(eq(N, Name, _)),
	assert(eq(N, Name, Val1)),
	!.

%%
%
%	eval
%	
%	
eval(B, _) :-
	dbg(['eval: ', B]), fail.

eval(B, B) :-
	number(B), !.

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
	
eval(call(B), B1) :-
	B =.. [Fname | Fargs],
	(   funcs(Fname, func(FargNames, Fcode))
	->  evalArr(Fargs, FargsEvaled),
	    level(N),
	    N1 is N+1,
	    retractall(level(_)),
	    assert(level(N1)),
	    setFuncVars(FargNames, FargsEvaled),
	    catch(run(Fcode),
		  return(Ret),
		  (   B1 = Ret
		  )
		 ),
	    retractall(level(_)),
	    assert(level(N))
	;   fail    
	),
	!.

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

eval(B, B1) :-
	atom(B),
	level(L),
	eq(L, B, B1),
	!.


eval(B, _) :-
	throw(interp_error(eval, B, '')),
	!.

evalArr([H | T], [Hevaled | Tevaled]) :-
	eval(H, Hevaled),
	evalArr(T, Tevaled),
	!.

evalArr([], []):-!.
	
%%	
%
%	runStmt
%	
%	
runStmt(A) :-
	dbg(['runStmt: ', A]), fail.

runStmt({}) :-!.
runStmt({Code}) :-
	run(Code),
	!.

runStmt(A=B) :-
	(   atom(A)
	->  eval(B, B1),
	    level(L),
	    retractall(eq(L, A, _)),
	    assert(eq(L, A, B1))
	;   errorRun([A, ' is not var'])),
	!.

runStmt(write(str(S))) :-
	write(S),
	!.
runStmt(write(A)) :-
	eval(A, A1),
	write(A1),
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

runStmt(function(Fname, Fargs, Fcode)) :-
	set(funcs(Fname, func(Fargs, Fcode))),
	!.

runStmt(return(B)) :-
	eval(B, B1),
	throw(return(B1)),
	!.

runStmt(Stmt) :-
	throw(interp_error(runStmt, Stmt, '')),
	!.

runSafe(Code) :-
	retractall(level(0)),
	assert(level(0)),
	catch(run(Code),
	      interp_error(Meth, Args, Msg),
	      (	  errorRun(['Method: "', Meth, '" with args: ', Args,': ', Msg])
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
	runSafe((
	    if(5<2, 
	       {
		write('true')
	       },
	       {
		write('false')
	       })
	    )).

test2 :-
	runSafe((
	   function(add, (a, b), {
			    return(a+b)
			    }),
	     c=2,
	     write(call(add(c, 5)))
	    )).

test1 :-
	runSafe(
	   (
	   a=10,
	    b=20+10/2,
	    c=a+b,
	    write(c)
	       
	   )
	   ).

test :-
	runSafe(
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

testFib :-
	runSafe((
		function(fib,n,
			{
			 if(n<2,
			    {
			     return(n)
			    },
			    {
			     return(call(fib(n-1))+call(fib(n-2)))
			    }
			   )
			}),
		 for(i=1,i=<10,i=i+1,
		    {
		     write(str('fib(')),write(i),write(str(')=')),write(call(fib(i))),write(str('\n'))
		    })
		
		)).

testFibDCG :-
	CodeString = "
	      function fib(n) 
	      {
	       if (n<2)
	       {
		return(n)
	       }
	       else
	       {
		return(fib(n-1)+fib(n-2))
	       }
	      };
	      for (i=1; i<=10; i=i+1)
	      {
	       write('fib(');
	       write(i);
	       write(')=');
	       write(fib(i));
	       write('\n')
	      }
	      ",
	write('Code:\n'),
	string_to_atom(CodeStringA, CodeString),
	write(CodeStringA),
	write('\nCompilling:\n'),
	phrase(code(Code), CodeString),
	write(Code),
	write('\nRunning...\n'),
	runSafe(Code),
	!.

code(Stmts) -->
	whitespace, statements(Stmts).

code({Code}) -->
	w([
	   "{", 
	   statements(Code), 
	   "}"
	  ]).

code({}) -->
	w([
	   "{",
	   "}"
	  ]).

statements((Stmt, Stmts)) -->
	w([
	   statement(Stmt), 
	   separator,
	   statements(Stmts)
	  ]),
	!.

statements(Stmt) -->
	statement(Stmt).

statement((A=B)) -->
	w([
	   identifier(A),
	   "=",
	   expression(B)
	  ]),
	!.

statement(function(Fname, Fargs, Fcode)) -->
	"function",
	whitespace,
	w([
	   identifier(Fname),
	   functionArgs(Fargs), 
	   code(Fcode)
	  ]),
	!.

statement(if(Cond, IfCode, ElseCode)) -->
	w([
	   "if",
	   parenthExpression(Cond),
	   code(IfCode),
	   elseStmt(ElseCode)
	  ]),
	!.

statement(for(Start, Cond, Step, Code)) -->
	w([
	   "for",
	   "(",
	   statements(Start),
	   ";",
	   expression(Cond),
	   ";",
	   statements(Step),
	   ")",
	   code(Code)
	  ]),
	!.

statement(write(Expr)) -->
	statement0(Expr, "write"),
	!.

statement(return(Expr)) -->
	statement0(Expr, "return"),
	!.

statement(call(Functor)) -->
	expression(call(Functor)).

statement0(Expr, Word) -->
	w([
	   Word,
	   parenthExpression(Expr)
	  ]).

elseStmt(ElseCode) -->
	w([
	   "else",
	   code(ElseCode)
	  ]).
elseStmt({}) --> [].

functionArgs(Fargs) -->
	w([
	   "(", 
	   identifiers(IdArr, ","), 
	   ")"
	  ]), 
	{ arrToTuple(Fargs, IdArr) }.

arrToTuple((A, B), [A | B1]) :-
	arrToTuple(B, B1), !.

arrToTuple(A, [A]).

identifiers([H | T], Separator) -->
	w([
	   identifier(H),
	   Separator,
	   identifiers(T, Separator)
	  ]),
	!.

identifiers([H], _) -->
	identifier(H).

identifier(A) -->
	[H | T],
	{
	 code_type(H, alpha),
	 (   alnum(T); T=""),
	 string_to_atom([H | T], A)
	}.

parenthExpression(Expr) -->
	w([
	   "(", 
	   expression(Expr), 
	   ")"
	  ]).

expression(Expr) -->
	w([
	   expression0(A),
	   operand(Op),
	   expression(Expr0)
	  ]),
	{Expr=..[Op, A, Expr0]}, !.
expression(Expr) -->
	expression0(Expr).

expression0(call(Functor)) -->
	w([
	   identifier(Ident),
	   parenthExpression(Expr)
	  ]),
	{Functor=..[Ident, Expr]}, 
	!.
expression0(I) --> integer(I), !.
expression0(str(S)) -->
	"'",[H | T],"'",
	{string_to_atom([H | T], S)},
	!.
expression0(A) --> identifier(A).

integer(I) -->
        digit(D0),
        digits(D),
        { number_chars(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D), !,
        digits(T).
digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.
	
operand(+) --> "+".
operand(-) --> "-".
operand(*) --> "*".
operand(/) --> "/".
operand(<) --> "<".
operand(>) --> ">".
operand(=<) --> "<=".
operand(>=) --> ">=".
operand(=:=) --> "==".
operand(=\=) --> "!=".

alnum([H | T]) :- code_type(H, alnum), alnum(T), !.
alnum([]).

separator -->
	( ";"; "\n"  ).

whitespace -->
	whitespace(_).

whitespace([H | T]) -->
	[H], whitespace(T),
	{code_type(H, space)}, !.
whitespace([]) -->
	[].

w([H | T]) -->
	H, whitespace, w(T).
w([]) -->
	[].




















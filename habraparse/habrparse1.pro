

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

alnum([H | T]) :- code_type(H, alnum), alnum(T), !.
alnum([]).

identifier(A) -->
    [H | T],
    {
     code_type(H, alpha),
     alnum(T),
     string_to_atom([H | T], A)
    }.

expression0(Expr) -->
    integer(Expr),
    !.

expression0(Expr) -->
    identifier(Expr),
    !.

expression0(Expr) -->
    "(",
    expression(Expr),
    ")".

operand(+) --> "+".
operand(-) --> "-".
operand(*) --> "*".
operand(/) --> "/".

expression(Expr) -->
    expression0(Expr0),
    operand(Op),
    expression(Expr1),
    {Expr =.. [Op, Expr0, Expr1]},
    !.

expression(Expr) -->
    expression0(Expr).

eval(Str, Vars, Res) :-
    %phrase(expression(Expr), Str),
    phrase(lex(Lexems), Str),
    format('Lexems: ~w~n', [Lexems]),
    phrase(expr(Expr), Lexems),
    format('Parsed: ~w~n', [Expr]),
    eval0(Expr, Vars, Res).

eval0(Expr, Vars, Res) :-
    Expr =.. [Op, Arg1, Arg2],
    eval0(Arg1, Vars, Arg1Evaled),
    eval0(Arg2, Vars, Arg2Evaled),
    Expr1 =.. [Op, Arg1Evaled, Arg2Evaled],
    Res is Expr1,
    !.

eval0(N, _, N) :- number(N), !.
eval0(K, Vars, V) :- member(K=V, Vars).

%
% lexer
%

lex([H | T]) -->
	term(H), !,
	lex(T).

lex([]) --> 
	[].

term(N) --> integer(N).
term(I) --> identifier(I).
term(open) --> "(".
term(close) --> ")".
term(+) --> "+".
term(-) --> "-".
term(*) --> "*".
term(/) --> "/".

%
% parser
%

:- dynamic [expr/5, expr0/5, num/3, oper/3, ident/3].


num(N) -->
	[N],
	{number(N)
	}.

oper(Op) -->
	[Op],
	{member(Op, [*,/,+,-])
	}.

ident(Id) -->
	[Id],
	{atom(Id)
	}.

expr0(E, [_,_ | A0], A) --> [open], expr(E, A0, A), [close], !.
expr0(N, [_ | A0], A0) --> num(N), !.
expr0(Id, [_ | A0], A0) --> ident(Id).

%expr(_, _, _, Lexems, _) :-
%	incr(lexpr), get_(lexpr, L),
%	tab(L), format('? ~w: ~w~n', [L,Lexems]), fail.

expr(Expr, [_ | A0], A) -->
	expr(Expr0, A0, A1),
	oper(Op),
	expr(Expr1, A1, A),
	{Expr =.. [Op, Expr0, Expr1]
	},
	%{get_(lexpr, L),
	% tab(L),format('+ ~w: ~w~n', [L,Expr]), decr(lexpr) 
	%},
	!.

expr(Expr, A0, A) --> expr0(Expr, A0, A).

%expr(_, _, _, Lexems, _) :-
%	get_(lexpr, L),
%	tab(L),format('- ~w: ~w~n', [L, Lexems]), decr(lexpr), fail.

e(Expr, Lexems) :-
	set(level, 0),
	phrase(expr(Expr, Lexems, []), Lexems).


:- dynamic [value/2].
set(A, B) :-
	retractall(value(A, _)),
	assert(value(A, B)).

get_(A, R) :-
	(   value(A, R); R = 0
	), !.

incr(A) :-
	get_(A, R),
	R1 is R+1,
	set(A, R1).

decr(A) :-
	get_(A, R),
	R1 is R-1,
	set(A, R1).

genVarList(Len, [_ | T]) :-
	Len > 0,
	Len1 is Len-1,
	genVarList(Len1, T),
	!.

genVarList(0, []).

decorateRule(Rule/Arity) :-
	genVarList(Arity, VL0),
	Head0 =.. [Rule | VL0],
	findall(clause(Head0, Body0), clause(Head0, Body0), Clauses),
	retractall(Head0),
	forall(member(clause(Head0, Body0), Clauses), 
	       (   assert((Head0:-(Body0, (
					  decr(level), get_(level, L),
					   tab(L),write(L), format('+ ~w ~w~n', [Rule, VL0])
						       ))))
			       )),
	
	genVarList(Arity, VL1),
	Head1 =.. [Rule | VL1],
	BeforeRule =.. [:-, Head1, (get_(level, L),
				  tab(L),write(L), format('? ~w ~w~n', [Rule, VL1]), incr(level),
				   fail
				  )],
	asserta(BeforeRule),
	
	genVarList(Arity, VL2),
	Head2 =.. [Rule | VL2],
	AfterRule =.. [:-, Head2, (decr(level), get_(level, L),
				   tab(L),write(L), format('- ~w ~w~n', [Rule, VL2]),
				   fail
				  )], 
	assertz(AfterRule)
	.

decorate([Rule/Arity | T]) :-
	decorateRule(Rule/Arity),
	decorate(T).

decorate([]).

doDecorate :-
	decorate([expr/5, expr0/5, num/3, oper/3, ident/3]).	

%:- initialization(doDecorate).



/*
expr1(E) --> 
	expr(A), 
	[Op], {member(Op, [+,-,*,/])}, 
	expr01(B), 
	{E =.. [Op, A, B]}, 
	!.

expr10(E) --> (expr1(E); expr0(E)), !.
expr01(E) --> (expr0(E); expr1(E)), !.

expr(E) -->
	expr10(E0),
	[Op], {member(Op, [+,-,*,/])},
	expr(E1),
	{E =.. [Op, E0, E1]},
	!.
expr(E) -->
	expr10(E),
	!.

a_op_b(A, Op, B) -->
	expr1(A),
	oper(Op),
	expr0(B).

expr1(A) -->
	[H | T],
	{expr(A, [H | T], [])
	}.

expr(E) -->
	a_op_b(A, Op, B),
	oper(Op1),
	expr(E1),
	{
	 AB =.. [Op, A, B],
	 E =.. [Op1, AB, E1]
	},
	!.
expr(E) -->
	a_op_b(A, Op, B),
	{E =.. [Op, A, B]
	},
	!.

expr(E) -->
	expr0(E).
*/

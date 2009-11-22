

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
    %phrase(expr(Expr), Lexems),
    e(Expr, Lexems),
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

expr0(E, [_,_ | A0], A) --> [open], expr(E, A0, A), [close].
expr0(N, [_ | A0], A0) --> num(N).
expr0(Id, [_ | A0], A0) --> ident(Id).

expr(Expr, [_ | A0], A) -->
	expr(Expr0, A0, A1),
	oper(Op),
	expr(Expr1, A1, A),
	{Expr =.. [Op, Expr0, Expr1]
	}, !.

expr(Expr, A0, A) --> expr0(Expr, A0, A).

e(Expr, Lexems) :-
	phrase(expr(Expr, Lexems, []), Lexems).



prior(+, 10).
prior(-, 10).
prior(*, 20).
prior(/, 20).

higher_prior(Op1, Op2) :-
	prior(Op1, P1),
	prior(Op2, P2),
	P1 > P2.


step(
    (	[H | T], Ln, Ls),
     (	 T, [H | Ln], Ls)
    ):- number(H).

step(
    (	[H | T]),
    (	[H | T])
    ).



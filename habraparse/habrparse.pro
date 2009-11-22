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
    phrase(expression(Expr), Str),
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
    

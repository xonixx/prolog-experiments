%
% helpful
%

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

%
% lexer
%

lex([H | T]) -->
	lexem(H), !,
	lex(T).

lex([]) -->
	[].

lexem(N) --> integer(N).
lexem(open) --> "(".
lexem(close) --> ")".
lexem(+) --> "+".
lexem(-) --> "-".
lexem(*) --> "*".
lexem(/) --> "/".
lexem(^) --> "^".

%
% parser
%

% aggregate
add_all(T1, [op_term(Op, T2)| T], R) :-
	T3 =.. [Op, T1, T2],
	!,
	add_all(T3, T, R).
add_all(R, [], R).

add_all_right(T1, [op_term(Op, T2)| T], R) :-
	add_all_right(T2, T, R1),
	!,
	R =.. [Op, T1, R1].
add_all_right(R, [], R).
% end aggregate


/*
 <expr> --> <term> ( '+' <term> | '-' <term> )*
 <term> --> <factor> ( '*'  <factor> | '/'  <factor> )*
 <factor> --> <pwr> ( '^' <pwr> )*
 <pwr> --> <prime> | '(' <expr> ')'
 <prime> --> ( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' )+
*/

% grammar

expr(E) --> term(T1), !, plus_minus_terms(Terms), {add_all(T1, Terms, E)}.

plus_minus_terms([op_term(Op, T1) | T]) --> plus_minus(Op), term(T1), !, plus_minus_terms(T).
plus_minus_terms([]) --> [].

plus_minus(+) --> [+].
plus_minus(-) --> [-].

term(T) --> factor(F1), !, mul_div_factors(Factors), {add_all(F1, Factors, T)}.

mul_div_factors([op_term(Op, F1) | T]) --> mul_div(Op), factor(F1), !, mul_div_factors(T).
mul_div_factors([]) --> [].

mul_div(*) --> [*].
mul_div(/) --> [/].

factor(F) --> pwr(P), !, pwrs(PP), {add_all_right(P, PP, F)}.

pwrs([op_term(Op, T1) | T]) --> pwr_op(Op), pwr(T1), !, pwrs(T).
pwrs([]) --> [].

pwr_op(^) --> [^].

pwr(E) --> [open], expr(E), [close].
pwr(N) --> [N], {number(N)}.
pwr(I) --> [I], {atom(I)}.


parse(Str, Expr) :-
	phrase(lex(Lexems), Str),
	phrase(expr(Expr), Lexems).





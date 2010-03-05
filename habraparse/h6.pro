
:- use_module(library(http/dcg_basics)).

%
% helpful
%
/*
ointeger(Val) -->
	digit(D0),
	digits(D),
	{ mkval([D0|D], 8, Val)
	}.

mkval([W0|Weights], Base, Val) :-
	mkval(Weights, Base, W0, Val).

mkval([], _, W, W).
mkval([H|T], Base, W0, W) :-
	W1 is W0*Base+H,
	mkval(T, Base, W1, W).
*/
%
% lexer
%

lex([H | T]) -->
	lexem_t(H), !,
	lex(T).

lex([]) -->
	[].

lexem_t(L) --> trashes, lexem(L), trashes.

trashes --> trash, !, trashes.
trashes --> [].

trash --> comment_marker(End), !, ..., End.
trash --> white.

comment_marker("*)") --> "(*".
comment_marker("*/") --> "/*".

... --> [].
... --> [_], !, (...).

hex_start --> "0X".
hex_start --> "0x".

/*
oct_start --> "0O".
oct_start --> "0o".
*/
lexem(open) --> "(".
lexem(close) --> ")".
lexem(+) --> "+".
lexem(-) --> "-".
lexem(*) --> "*".
lexem(/) --> "/".
lexem(^) --> "^".

lexem(N) --> hex_start, !, xinteger(N). % this handles hex numbers
lexem(N) --> number(N). % this handles integers/floats
%lexem(N) --> oct_start, !, ointeger(N). % this handles oct numbers

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
 <pwr> --> <prime> | '(' <expr> ')' | '-' <factor> | '+' <factor>
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
pwr(F) --> plus_minus(Op), factor(F0), {F =.. [Op, F0]}.


parse(Str, Expr) :-
	phrase(lex(Lexems), Str),
	phrase(expr(Expr), Lexems).


%
%tst
%

test(S) :-
	format('~n-------~nInput string is:~n~s~n~n', [S]),
	parse(S, R),
	Res is R,
	format('Parsed expression is:~n~w~n~nResult is:~n~w', [R, Res]).
tests :-
	forall(member(S, [
			  "   142 /* some cool comment /*??? /* yo! */ - 0x2A \t\t  + 1.6e+3\t ^ ((2 -(* some other comment 1/0 *) 1)*0.5)  \t",
			  "1000000+1000000+1000000+1000000+1000000+1000000+1000000+10000000+1000000+1000000"
			 ]), test(S)).

tests1(N, Res) :-
   phrase(n(N, "+1000000"),S1), /*test(S1)*/parse(S1,R), Res is R.

n(N, S) --> {N>0}, S, !, {N1 is N - 1}, n(N1, S).
n(0, _) --> [].

tst(N, Res) :- phrase(n(N, "+1"),S), parse(S, R), /*Res is R*/c(R, Res).

e(N, E) :-
	N > 0,
	E = E1 + 1,
	N1 is N - 1,
	e(N1, E1).
e(0, 1).


c(A+B, R) :- c(A, Ra), c(B, Rb), !, R is Ra + Rb.
c(A-B, R) :- c(A, Ra), c(B, Rb), !, R is Ra - Rb.
c(A*B, R) :- c(A, Ra), c(B, Rb), !, R is Ra * Rb.
c(A/B, R) :- c(A, Ra), c(B, Rb), !, R is Ra / Rb.
c(A^B, R) :- c(A, Ra), c(B, Rb), !, R is Ra ^ Rb.
c(A, A).

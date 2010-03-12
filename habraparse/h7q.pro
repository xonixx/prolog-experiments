
:- use_module(library(http/dcg_basics)).

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

lexem(open) --> "(".
lexem(close) --> ")".
lexem(+) --> "+".
lexem(-) --> "-".
lexem(*) --> "*".
lexem(/) --> "/".
lexem(^) --> "^".

lexem(N) --> hex_start, !, xinteger(N). % this handles hex numbers
lexem(N) --> number(N). % this handles integers/floats

%
% parser
%

/*
 <expr> --> <term> ( '+' <term> | '-' <term> )*
 <term> --> <factor> ( '*'  <factor> | '/'  <factor> )*
 <factor> --> <pwr> ( '^' <pwr> )*
 <pwr> --> <prime> | '(' <expr> ')' | '-' <factor> | '+' <factor>
 <prime> --> ( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' )+
*/

% grammar

expr(Res) --> term(T), !, plus_minus_terms(T, Res).

plus_minus_terms(T, Res) --> plus_minus(Op), !, term(T1), {Op=(+)->T2 is T+T1;T2 is T-T1}, !, plus_minus_terms(T2, Res).
plus_minus_terms(T, T) --> [].

plus_minus(+) --> [+].
plus_minus(-) --> [-].

term(Res) --> factor(F), !, mul_div_factors(F, Res).

mul_div_factors(F, Res) --> mul_div(Op), !, factor(F1), {Op=(*)->F2 is F*F1;F2 is F/F1}, !, mul_div_factors(F2, Res).
mul_div_factors(F, F) --> [].

mul_div(*) --> [*].
mul_div(/) --> [/].

factor(F) --> pwr(P), !, pwrs(PP), {F is P^PP}.

pwrs(Res) --> pwr_op, pwr(T1), !, pwrs(T), {Res is T1^T}.
pwrs(1) --> [].

pwr_op --> [^].

pwr(E) --> [open], expr(E), [close].
pwr(N) --> [N], {number(N)}.
pwr(R) --> plus_minus(Op), factor(F0), {F =.. [Op, F0], R is F}.


parse(Str, Expr) :-
	writeln(1),
	phrase(lex(Lexems), Str),
	writeln(2),
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

tst(N, Res) :- phrase(n(N, "+1"),S), time(parse(S, Res)).%, /*Res is R*/c(R, Res).

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


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

trash --> comment_marker(End), !, string(_), End.
trash --> white.

comment_marker("*)") --> "(*".
comment_marker("*/") --> "/*".

hex_start --> "0X".
hex_start --> "0x".

lexem(open) --> "(".
lexem(close) --> ")".
lexem(open) --> "[".
lexem(close) --> "]".
lexem(+) --> "+".
lexem(-) --> "-".
lexem(*) --> "*".
lexem(/) --> "/".
lexem(^) --> "^".
lexem(,) --> ",".
lexem(!) --> "!".

lexem(N) --> hex_start, !, xinteger(N). % this handles hex numbers
lexem(N) --> number(N). % this handles integers/floats
lexem(var(A)) --> identifier_c(L), {string_to_atom(L, A)}.

identifier_c([H | T]) --> alpha(H), !, many_alnum(T).

alpha(H) --> [H], {code_type(H, alpha)}.
alnum(H) --> [H], {code_type(H, alnum)}.

many_alnum([H | T]) --> alnum(H), !, many_alnum(T).
many_alnum([]) --> [].

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
 <pwr> --> <a> <post_function> | <a>
 <a> --> <prime>
	 | <function_name> '(' <expr> ( ',' <expr> )*  ')'
	 | '(' <expr> ')'
	 | '-' <factor>
	 | '+' <factor>
 <prime> --> ( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' )+
 <post_function> --> '!'
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

args([E1 | E]) --> expr(E1), !, comma_args(E).

comma_args([E1 | E]) --> [,], expr(E1), !, comma_args(E).
comma_args([]) --> [].

post_f(fact) --> [!].
post_f(sin) --> [var(sin)].
post_f(tan) --> [var(tan)].

pwr(P) --> a(P0), post_f(Fn), {P =.. [Fn, P0]}, !.
pwr(P) --> a(P).

a(F) --> [var(Fn)], [open], args(A), [close], {F =.. [Fn | A]}.
a(E) --> [open], expr(E), [close].
a(N) --> [N], {number(N)}.
a(F) --> plus_minus(Op), factor(F0), {F =.. [Op, F0]}.
a(E) --> [E].

parse(Str, Expr) :- parse(Str, Expr, []).

parse(Str, Expr, Vars) :-
	phrase(lex(Lexems), Str),
	phrase(replace(Lexems1, Vars), Lexems),
	phrase(expr(Expr), Lexems1).

replace([H | T], Vars) -->
	[L],
	{ ((L = var(L1), member(L1=V, Vars))
	  ->  parse(V, H, Vars)
	  ;   L=H
	  )
	},
	!,
	replace(T, Vars).
replace([], _) --> [].

:- arithmetic_function(lg/1).
:- arithmetic_function(ln/1).
:- arithmetic_function(fact/1).

lg(A, R) :- R is log10(A).
ln(A, R) :- R is log(A).

fact(A, 1) :- A < 2,!.
fact(A, R) :- R is A * fact(A - 1).


%
%tst
%
tst1 :-
	S = "8! + lg(r)^4 - w",
	parse(S, R, [w="5*ln(r)", r="77.8"]),Res is R,
	format('input: ~s~nparsed: ~w~ncalculated: ~w~n', [S, R, Res]).

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

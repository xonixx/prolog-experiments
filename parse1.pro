

% process(Inp, Remaining, Parsed)

ischar(H, [H]).

char_is(digit, C, Digit) :-
	member([C]/Digit, 
	       [
		"0"/0, "1"/1, "2"/2, "3"/3, "4"/4, 
		"5"/5, "6"/6, "7"/7, "8"/8, "9"/9
	       ]), !.

char_is(sign, C, Sign) :-
	member([C]/Sign, 
	       [
		"+"/(+), "-"/(-), "*"/(*), "/"/(/)
	       ]), !.


process(Inp, Out, expr(Sign, D, Expr)) :-
	process(Inp, Inp1, D), D = digits(_), 
	process(Inp1, Inp2, sign(Sign)),
	process(Inp2, Out, Expr),
	(   Expr = expr(_)
	;   Expr = digits(_)
	), !.

process([H | T], R, digits([Dh | Dt])) :-
	char_is(digit, H, Dh), !,
	(   process(T, R, digits(Dt))
	;   Dt = []	
	), !.

process([H | T], T, sign(Sign)) :-
	char_is(sign, H, Sign), !.

process(Inp, Inp, nil).

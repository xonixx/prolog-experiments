
:- set_prolog_flag(float_format,'%.15g').

many1(What, [H | T]) -->
	call(What, H), !,
	%{P =.. [What, H]},
	%P, !,
	many1(What, T).
many1(_, []) --> [].


integer(I) -->
        many1(digit, [D|DD]),
	{ number_chars(I, [D|DD])
        }.

digit(D) -->
        [D],
        { code_type(D, digit)
        }.


float(F) -->
	(   "-", {Sign = -1}
	;   "", {Sign = 1}
	), !,
	integer(N),
	",",
	integer(D),
	{F is Sign * (N + D / 10^(ceiling(log10(D))))
	}.

sum(S, Total) -->
	float(F1), !,
	" ",
	{ S1 is S + F1},
	sum(S1, Total).
sum(Total, Total) -->
	[].

go1 :-
	phrase_from_file(sum(0, S),'numbers_large.txt', [buffer_size(16384)]),
	writeln(S).

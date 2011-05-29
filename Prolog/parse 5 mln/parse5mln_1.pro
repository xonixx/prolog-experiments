
:- set_prolog_flag(float_format,'%.15g').

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


float(F) -->
	(   "-", {Sign = -1}
	;   "", {Sign = 1}
	), !,
	integer(N),
	",",
	integer(D),
	{F is Sign * (N + D / 10^(ceiling(log10(D))))
	}.


sum(S) -->
	float(F1), !,
	" ",
	(   sum(S1)
	;   {S1 = 0}
	),
	{ S is F1 + S1}.


go :-
	read_file_to_codes('numbers_large.txt',Codes,[]),
	writeln('Processing...'),
	sum(S,Codes,[]),
	writeln(S).

s :- 
	open(user, read, Stream), 
	set_input(Stream), 
	start.
start :- 
	current_input(Stream),
	read_stream_to_codes(Stream, Codes),
	phrase(sum(S), Codes),
	format('Sum is ~d', [S]).

nl --> [C], {char_type(C, newline)}.

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

sum(S) -->
	integer(N),
	nl,
	!,
	sum(S1),
	{S is S1 + N}.
sum(N) --> integer(N), 
	([]; nl),
	!.


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
    (   "-", {Sign =3D -1}
    ;   "", {Sign =3D 1}
    ),
    integer(N),
    ",",
    integer(D),
    {F is Sign * (N + D / 10^(ceiling(log10(D))))
    }.

sum(S, Total) -->
    float(F1), !,
    " ",
    { S1 is S + F1},
    sum(S1, Total),
    !.
sum(Total, Total) -->
    [].

go1 :-
    phrase_from_file(sum(0, S),'numbers_large.txt', [buffer_size(16384)]),
    writeln(S).

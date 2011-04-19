
spaces --> " ", may_be_spaces.

may_be_spaces --> " ", may_be_spaces.
may_be_spaces --> "".

prop([A|S]) --> [A], {is_prop_sybmol(A)}, prop(S).
prop([]) --> [].

is_prop_sybmol(X) :- is_alnum(X); [X]="_". %символ алфавита либо символ подчеркивания

nl --> "\r\n", !.
nl --> "\n".

type_pattern(TypeName) --> "type", spaces, prop(TypeName).

prop_val(PropName, ValName) --> prop(PropName), spaces, prop(ValName).

many_prop_val([P=V|T]) --> prop_val(P, V), nl, many_prop_val(T).
many_prop_val([P=V]) --> prop_val(P, V).

unit(Type, PV) --> type_pattern(Type), !, nl, many_prop_val(PV).

units([unit(Type,PV)|T]) --> unit(Type, PV), nl, nl, !, units(T).
units([unit(Type,PV)]) --> unit(Type, PV).

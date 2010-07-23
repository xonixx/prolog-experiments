p([_,X],[X]).
p([H|Tail],[H|NewTail]):-p(Tail,NewTail).


positive(1).
positive(X) :- positive(Y), X is Y + 1.

positive1(X) :- from(1,X).

from(From,From).
from(From,X):-From1 is From + 1, from(From1,X).


:-use_module(library(chr)).
:- chr_constraint(pos/1).

pos(1) <=> true.
pos(X) ==> Y is X+1, pos(Y).

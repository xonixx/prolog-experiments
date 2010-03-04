
all([N|T], N) :- all(T,N).
all([],_).


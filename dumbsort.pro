sorted([]).
sorted([_]).
sorted([X,Y|T]) :- X =< Y, sorted([Y|T]).


my_delete([X|Xs], X, Xs).
my_delete([Y|Ys], X, [Y|Ys1]) :- my_delete(Ys, X, Ys1).

my_add(Xs, X, Ys) :- my_delete(Ys, X, Xs).

shuffle([], []).
shuffle([X|Xs], Ys) :-
        shuffle(Xs, Xs1),
        my_add(Xs1, X, Ys).

my_sort(Unsorted, Sorted) :-
        shuffle(Unsorted, Sorted),
        sorted(Sorted).

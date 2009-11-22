
aaa(bbb(25/47)).
aaa(bbb(36)).

get_space(Space) :-
 [Space] = " ".

num_words("", 0) :-!.

num_words(S, 1) :-
 get_space(Space),
 \+ member(Space, S),
 !.
 
num_words(S, N) :-
 get_space(Space),
 append(A, [Space | S1], S),
 \+ member(Space, A),
 num_words(S1, N1),
 N is N1+1.

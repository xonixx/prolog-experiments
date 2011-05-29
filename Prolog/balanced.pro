
many(What) -->
	What,
	many(What).
many(_) --> [].

bracketed(Open, Close, What) -->
	Open,
	What,
	Close.

braced(What) --> bracketed("(", ")", What).
braced(What) --> bracketed("{", "}", What).
braced(What) --> bracketed("[", "]", What).

balanced -->
	many(braced(balanced)).

test :-
	phrase(balanced, "[](){[{()}]}"),
	phrase(balanced, "[[[()]]]({}{{[]}})[{}]()[]"),
	\+ phrase(balanced, "[[[()]]]({[}{{[]}})[{}]()[]").


oneOf(L, X) --> {member(X, L)}, X.

noIng(NoIng) --> oneOf([[_|_], []], NoIng).

endOfWord(E) --> oneOf([" ", ",", "?", "!", ";", "."], E).

ing2ed(R) -->
	noIng(NoIng),
	"ing",
	endOfWord(E), !,
	ing2ed(Ed), !,
	{
	append([NoIng, "ed", E, Ed], R)
	}.

ing2ed(R) --> noIng(NoIng), "ing", !,
	{
	append(NoIng,"ed",R)
	}.

ing2ed(NoIng) --> noIng(NoIng),!.

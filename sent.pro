
char(C, OfTypes) -->
	[C],
	{
	char_type(C, Type),
	 memberchk(Type, OfTypes)
	}.

space_punct --> char(_, [white, space, punct]).

spaces_puncts -->
	space_punct, !,
	(   spaces_puncts; []).
spaces_puncts -->space_punct.

end --> ".".
end --> "!".
end --> "?".

letter(L) --> char(L, [alnum]).
letters(W) -->
	letter(L), !,
	letters(LL),
	{W=[L|LL]}.
letters([])-->[].

word(W) --> letters(LL), {name(W,LL)}.

words([W|WW]) -->
	word(W),
	!,
	spaces_puncts,
	(   words(WW); {WW=[]}).

words([W]) --> word(W).

sentence(W) --> words(W), end.






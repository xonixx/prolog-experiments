

sort1([], []) :-!.
sort1([H | T], Res) :-
	findall(A, (member(A, T), H<A), HMore),
	findall(B, (member(B, T), H>=B), HLess),
	sort1(HMore, HMoreSorted),
	sort1(HLess, HLessSorted),
	append(HLessSorted, [H | HMoreSorted], Res).

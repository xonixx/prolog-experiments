
bracket("]") --> "[".
bracket(")") --> "(".
bracket("}") --> "{".

brackets --> bracket(Close), brackets, Close, brackets.
brackets --> [].

check(BracesStr) :-
	phrase(brackets, BracesStr), !.

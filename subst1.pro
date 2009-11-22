

subst(Str, What, ToWhat, Res) :-
	append(What, AfterWhat, WhatAfterWhat),
	append(Pre, WhatAfterWhat, Str), !,
	subst(AfterWhat, What, ToWhat, Res1),
	append(Pre, ToWhat, PreToWhat),
	append(PreToWhat, Res1, Res).

subst(Str, _, _, Str).
	



subst(Str, What, ToWhat, Res) :-
	append(What, AfterWhat, WhatAfterWhat),
	append(Pre, WhatAfterWhat, Str), !,
	subst(AfterWhat, What, ToWhat, Res1),
	append(Pre, ToWhat, PreToWhat),
	append(PreToWhat, Res1, Res).

subst(Str, _, _, Str).


subst1(Str,What,ToWhat,Res) :-
	append([Pre, What, AfterWhat], Str), !,
	subst1(AfterWhat,What,ToWhat,Res1),
	append([Pre, ToWhat, Res1], Res).
subst1(Str,_,_,Str).
	

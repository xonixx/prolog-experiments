
:- use_module(library(clpq)).

solve(Res) :-
	Res = [olimpiada/_/CupsO, sosipatra/_/CupsS, polixena/_/CupsP],
	{CupsO + 5 = CupsS + CupsP,
	 CupsS + 9 = CupsO + CupsP},

	select(_/karpovna/11, Res, L1),
	select(_/titovna/CupsT, L1, [_/uvarovna/_]), CupsT mod 3 =:= 0.


p(J,J,"Uvarovna").
p(J,J,"Titovna"):- J mod 30=:=0.
p(110,110,"Karpovna").
p(I,J,B):- I<200, U is I+1, p(U,J,B).

tst:-
p(1,A,B), p(1,C,D), B\=D, p(1,E,F), F\=B, F\=D, A+50=:=C+E, C+90=:=A+E,
format("Olimpiada ~s ~d~nSosipatra ~s ~d~nPoliksena ~s ~d~n",[B,A,D,C,F,E]),fail.
tst.

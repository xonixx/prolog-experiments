/*
l(S,L):-append(_,[A,B],S),(B=L;A=L).
s(I,O):-select(A,I,Y),s(A,Y,O),!.
s(A,I,[A|O]):-select(B,I,Y),B=[F|_],G is F+32,l(A,G),s(B,Y,O).
s(_,[],[]).
*/
/*
l(S,L):-append(_,[A,B],S),(B=L;A=L).
s(I):-select(A,I,Y),s(A,Y,O),maplist(name,R,O),write(R),!.
s(A,I,[A|O]):-select(B,I,Y),B=[F|_],G is F+32,l(A,G),s(B,Y,O);O=[].
*/
/*
l(K,S,L):-append(_,[A,B],S),(B=L;K=2->A=L).
s(I):-(K=1;K=2),select(A,I,Y),s(K,A,Y,O),maplist(name,R,O),write(R),!.
s(K,A,I,[A|O]):-select(B,I,Y),B=[F|_],G is F+32,s(K,B,Y,O),l(K,A,G);I=[],O=I.
*/

/*
l(K,S,L):-append(_,[A,B],S),(B=L;K=2,A=L).
s(I,O):-(K=1;K=2),select(A,I,Y),s(K,A,Y,O),!.
s(K,A,I,[A|O]):-select(B,I,Y),B=[F|_],G is F+32,l(K,A,G),s(K,B,Y,O);I=[],O=I.
*/

:-use_module(library(clpfd)).

% solution, length=162
l(K,S,L):-append(_,[A,B],S),(B=L;K=2,A=L).
s(I,O):-(K=1;K=2),select(A,I,Y),s(K,A,Y,O).
s(K,A,I,[A|O]):-select(B,I,Y),B=[F|_],G#=F+32,l(K,A,G),s(K,B,Y,O);I=[],O=I.
% end solution

tst :-
	I1=["Калининград", "Вологда", "Алматы", "Дмитров",
	   "Архангельск", "Тобольск", "Краков"],
	I2=["Калининград","Кондопога","Александров","Всеволожск"],
	I3=["Калининград", "Вологда", "Далматово", "Дмитров", "Архангельск", "Владивосток", "Краков"],
	I4=["Yz", "Zy", "Xyz"],
	forall(member(L,[I1,I2,I3,I4]),
	       (s(L,O),
		maplist(string_to_atom,O,R),
		writeln(R))).

/* runing
 ?- [golf].
% golf compiled 0.02 sec, 88 bytes
true.

 ?- tst.
[Алматы,Тобольск,Калининград,Дмитров,Вологда,Архангельск,Краков]
[Кондопога,Александров,Всеволожск,Калининград]
[Дмитров,Вологда,Архангельск,Краков,Владивосток,Калининград,Далматово]
[Xyz,Zy,Yz]
true.

*/

member(H,[H|_]).
member(H,[_|Tail]):-member(H,Tail).

append([],B,B).
append([H|Tail],B,[H|NewTail]):-append(Tail,B,NewTail).

insert(XZ,Y,XYZ):-append(X,Z,XZ),append(X,[Y],XY),append(XY,Z,XYZ).

permute([],[]).
permute([H|Tail],P):-permute(Tail,Tail1),insert(Tail1,H,P).


solve:-
    permute([inostrannyj_jazyk,fizika,himija,biologija,literatura,matematika],[S1,S2,S3,S4,S5,S6]),
    permute([1,2,3,4,5,6],[L1,L2,L3,L4,L5,L6]),
    List=[l(S1,L1),l(S2,L2),l(S3,L3),l(S4,L4),l(S5,L5),l(S6,L6)],
    rule1(List),rule2(List),rule3(List),rule4(List),rule5(List),rule6(List),rule7(List),rule8(List),
    write(["Antonov - ",S1,' ',L1]),nl,write(["Vasilev - ",S2,' ',L2]),nl,write(["Stepanov - ",S3,' ',L3]),nl,
    write(["Dubinin - ",S4,' ',L4]),nl,write(["Elizarov - ",S5,' ',L5]),nl,write(["Fedorov - ",S6,' ',L6]),nl.


% Esli Stepanov ne prepodaet himiju, to uzh tochno biologiju; on zhe zanjat na tret'em uroke v vos'mom klasse.
rule1([_,_,l(himija,L),_,_,_]):-L \= 3.
rule1([_,_,l(biologija,L),_,_,_]):-L \= 3.

% Fedorov ne literator, na vtorom uroke prisutstvovat' ne mozhet.
rule2([_,_,_,_,_,l(S,L)]):-not(S=literatura),L \= 2.

% Pjatyj urok provodit esli ne Vasil'ev, to Dubinin.
rule3([_,l(_,5),_,_,_,_]).
rule3([_,_,_,l(_,5),_,_]).

% Antonov i Vasil'ev prepodajut inostrannyj jazyk ili fiziku.
rule4([l(inostrannyj_jazyk,_),l(fizika,_),_,_,_,_]).
rule4([l(fizika,_),l(inostrannyj_jazyk,_),_,_,_,_]).

% Na chetvertom uroke – himija, fizika ili inostrannyj jazyk, no prepodavateli ne Antonov i ne Vasil'ev.
rule5(List):-member(l(himija,4),List),not(List=[l(_,4),_,_,_,_,_]),not(List=[_,l(_,4),_,_,_,_]).
rule5(List):-member(l(fizika,4),List),not(List=[l(_,4),_,_,_,_,_]),not(List=[_,l(_,4),_,_,_,_]).
rule5(List):-member(l(inostrannyj_jazyk,4),List),not(List=[l(_,4),_,_,_,_,_]),not(List=[_,l(_,4),_,_,_,_]).

% Na shestom uroke inostrannyj jazyk, himija ili literatura: prepodavateli – Vasil'ev ili Stepanov.
rule6_1(List):-member(l(himija,6),List);member(l(literatura,6),List);member(l(inostrannyj_jazyk,6),List).
rule6_2(List):-List=[_,l(_,6),_,_,_,_];List=[_,_,l(_,6),_,_,_].
rule6(List):-rule6_1(List),rule6_2(List).

% Matematik, schitaja (soglasno ucheniju Pifagora!) chetnye chisla «schastlivymi», prosil dat' emu «chetnyj» urok.
rule7(List):-member(l(matematika,L),List),L mod 2 =0.

% Biologicheskij kabinet osvobozhdaetsja ne ranee vtorogo uroka.
rule8(List):-member(l(biologija,1),List);member(l(biologija,2),List).

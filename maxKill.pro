
setFigs([Figa|Fh],[Boi|Bh],[Kletki|Klh],Pole,Bitye,[Fpoz|Poza]):- 
	Figa \= n,
	setFigs(Fh,Bh,Klh,Po,Bit,Poza), 
	member_(Kletki,Fpoz,_),
	\+ member_(Poza,Fpoz,_), 
	countNew(Figa,Fpoz,Boi,Poza,Po,Pole,Bit,Bitye).
setFigs([n,n|Fh],[Boi|Bh],[Kletki|Klh],Pole,Bitye,[Fp2,Fp1|Poza]):-
	setFigs(Fh,Bh,Klh,Po,Bit,Poza),
	member_(Kletki,Fp1,Klet), 
	\+ member_(Poza,Fp1,_),
	countNew(n,Fp1,Boi,[],Po,Pole0,Bit,Bitye0),
	member_(Klet,Fp2,_), 
	\+ member_(Poza,Fp2,_),
	countNew(n,Fp2,Boi,[],Pole0,Pole,Bitye0,Bitye).    
setFigs([],_,_,
       [2,1,252,252,252,252,252,252],
	26,[]).

/******************************************************* */
countNew(Figa,Kletka,[B|Bh],Poza,Po,Pole,Bit,Bitye):-!,
	countB(1,Figa,Kletka,B,Poza,Po,PoN,Bit,BitN),!,
	countNew(Figa,Kletka,Bh,Poza,PoN,Pole,BitN,Bitye).
countNew(_,_,[],_,X,X,Y,Y).

/******************************************************* */
countB(Sh,Figa,Kl,Boi,Poza,P,Pv,B,Bv):- 
	check(Sh,Figa),
	KlN is Kl+Boi, 
	KlN < 89, 
	KlN > 10, 
	KlD is KlN mod 10, 
	KlD =\= 0, 
	KlD =\= 9,
	!,
	setZero(KlN,P,Pn,B,Bn), 
	ifFiga(Sh,KlN,Poza,ShN),
	!,
	countB(ShN,Figa,KlN,Boi,Poza,Pn,Pv,Bn,Bv).
countB(_,_,_,_,_,X,X,Y,Y).

ifFiga(_,Kl,Poza,-1):- member_(Poza,Kl,_),!.
ifFiga(Sh,_,_,ShN):- ShN is Sh+1.

check(-1,_):-!,fail.
check(2,k):-!,fail.
check(2,n):-!,fail.
check(_,_).

/********************************************************/
setZero(Kl,[G|H1],[G|H2],B,Bn):- 
	Kl>20,
	!, 
	K is Kl-10,
	!, 
	setZero(K,H1,H2,B,Bn).
setZero(Kl,[C|H],[Cn|H],B,Bn):- 
	bits(Kl,Z), 
	Cn is C /\ Z,
	del(C,Cn,B,Bn).

del(X,X,Y,Y):-!.
del(_,_,X,Y):- Y is X+1.

bits(11,254).
bits(12,253).
bits(13,251).
bits(14,247).
bits(15,239).
bits(16,223).
bits(17,191).
bits(18,127).

member_(A,B,C):-select(B,A,C).

go :-
	KlQ=[33,34,35,36,37,38,44,45,46,47,48,55,56,57,58,66,67,68,77,78,88],
	KlB=[44,46,55,57,64,66,75,77],
	KlW=[45,47,54,56,65,67,74,76],
	KlN=[44,45,46,47,54,55,56,57,64,65,66,67,74,75,76,77],
	KlK=[44,45,46,47,54,55,56,57,64,65,66,67,74,75,76,77],
	Kletki=[KlQ,KlB,KlW,KlN,KlK],
	
	BoiQ=[-11,-10,-9,-1,1,9,10,11],
	BoiB=[-11,-9,9,11],
	BoiN=[-21,-19,-12,-8,8,12,19,21],
	BoiVse=[BoiQ,BoiB,BoiB,BoiN,BoiQ],
		
	Figs=[q,b,w,n,n,k],

	setFigs(Figs,BoiVse,Kletki,_,Bac,Poza), 
	Bac=63,
	!,
	write(Poza).
	
	

	
	





:- use_module(library(clpfd)).
:- use_module(library(clpr)).

test(C) :-
	findall(Q,
	      (
	      N in 1..C, label([N]),
	       {Q = N/C, Q>=0.3, Q=<0.7}
	      ),
		L), write(L).

test2(L) :-
	N=5,findall(E,(from_to(1,N,R), E1 is R/N, (E1<0.3->E=0.3;E1>0.7->E=0.7;E=E1)),L).

from_to(From, From, From) :-!.
from_to(From, To, From) :- From \= To.
from_to(From, To, From2) :- From1 is From + 1, from_to(From1, To, From2).


������(In, Out, N) :- ������(In, Out, 0, N).

������("","", N, N) :-!.
������([������ | ����1], [���� | ����], N, O):-
	(����([������], [����])
	->  N1 is N + 1
	;   N1 = N,
	    ����=������
	),
	������(����1, ����, N1, O).

����("�","A"). ����("�","e"). ����("�","o"). ����("�","p").
����("�","c"). ����("�","x"). ����("�","y").


��������([����� | ����]) -->
	[����],
	{����([����], [�����])
	 ; ����� = ����
	},
	!,
	��������(����).
��������([]) --> [].

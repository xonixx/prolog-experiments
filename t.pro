p(_,0):- !.
p(A,N):- write(A),N1 is N-1,p(A,N1).

r(_,_,0):- !.
r(A,N,K):- p(A,N),nl,K1 is K-1,r(A,N,K1).

w(_,_,_,0):- !.
w(A,B,N,K):- p(A,N),p(B,N),nl,K1 is K-1,w(A,B,N,K1).

t(K,N):-
    N1 is 3*N, r('*',N1,K),
    K1 is 2*K, w(' ','*',N,K1).

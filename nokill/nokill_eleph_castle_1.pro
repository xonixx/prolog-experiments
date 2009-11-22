
:- use_module(library(clpfd)).

make_sol([F | FF],[X | XX],[Y | YY], [{F,X,Y} | SS]) :- make_sol(FF,XX,YY,SS).
make_sol([],[],[],[]).

range([From | T], From, To) :- 
	From < To, !,
	F1 is From + 1,
	range(T, F1, To).
range([To],To,To).

nokill([H | T]) :-
	nokill(H, T), !,
	nokill(T).
nokill([]).

nokill(F1, [F2 | FF]) :-
	nokill_2(F1, F2), !,
	nokill(F1, FF).
nokill(_, []).

nokill_2({1,X1,Y1}, {1,X2,Y2}) :-
	abs(X1 - X2) #\= abs(Y1 - Y2),
	abs(X1 - X2) * abs(Y1 - Y2) #\= 2. % horse
nokill_2({2,X1,Y1}, {2,X2,Y2}) :-
	X1 #\= X2,
	Y1 #\= Y2,
	abs(X1 - X2) * abs(Y1 - Y2) #\= 2. % horse

nokill_2({1,K1}, {2,K2}) :-
	nokill_2({1,K1},{1,K2}),
	nokill_2({2,K1},{2,K2}).
nokill_2(X, Y) :- nokill_2(Y, X).
	

solve(S, N) :-
	length(FF, N),
	range(XX, 1, N),
	length(YY, N),

	N32 is 3*N//2,

	FF ins 1..2, % 1 - elephant, 2 - castle
	sum(FF, #=, N32),
	
	YY ins 1..N, all_different(YY),
	
	make_sol(FF,XX,YY,S),
	
	nokill(S),
	
	append([FF,XX,YY],Vars), 
	label(Vars).

line(N,X,F,Xc,[C | CC]) :-
	Xc=<N, !,
	(   Xc = X
	->  C = F
	;   C = ' '
	),
	Xc1 is Xc + 1,
	line(N,X,F,Xc1,CC).
line(_,_,_,_,[]).

format_line([H | T]) :-
	write(H),
	write('|'), !,
	format_line(T).
format_line([]).

empty_line(N,Cn) :-
	Cn=<N, !,
	write('-'),
	Cn1 is Cn + 1,
	empty_line(N,Cn1).
empty_line(_,_).

lines(Line,S,N) :-
	Line=<N,
	member({F,X,Line},S), !,
	line(N,X,F,1,LineArr),
	format_line(LineArr),nl,
	N2 is N * 2,
	empty_line(N2,1),nl,
	L1 is Line + 1,
	lines(L1,S,N).
lines(_,_,_).
	

draw(S) :-
	length(S, N),
	lines(1,S,N).

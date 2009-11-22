:- use_module(library(clpfd)).
:- use_module(tokenizer).

makeVars([H | T], [V | Vt], [H=V | Hvt]) :- makeVars(T,Vt,Hvt).
makeVars([],[],[]):-!.

substList(L, [E | EE], [VE | VEE]) :-
	subst(L, E, VE),
	substList(L, EE, VEE).
substList(_,[],[]):-!.

subst(L, E, V) :-
	member(E=V, L),
	!.
subst(L, E, V) :- 
	E =.. [H | T],
	substList(L, T, VT),
	V =.. [H | VT],
	!.
subst(_,E,E).

do(L, F) :-
	subst(L, F, VF),
	VF.

solve(FileName) :-
	read_file_to_codes(FileName,Codes, []),
	string_to_atom(Codes,Txt),
	format('Solving:~n~n~w~n~n', Txt),
	writeln('Lexing...\n'),
	tokenizer:token_list(TList, Codes, []),
	writeln(TList),nl,
	writeln('Parsing...\n'),
	program(P,TList,[]),
	writeln(P),nl,
	doProgram(P, Solution),
	writeln('Solution:\n'),
	writeln(Solution).

test :-
	solve('einstain.txt'),
	solve('nums.txt').

makeDomain([N1,N2 | NN], N1 \/ N2 \/ NND) :-
	makeDomain(NN,NND).
makeDomain([N1,N2], N1 \/ N2):-!.
makeDomain([N],N):-!.

doDomains(L, [domain(Vars,Nums) | DD]) :-
	makeDomain(Nums,NumsDomain),
	do(L, Vars ins NumsDomain),
	do(L, all_distinct(Vars)),
	doDomains(L, DD).
doDomains(_, []).

q(=,#=).
q(<,#<).
q(>,#>).

doConditions(L, [C | CC]) :-
	q(Op,Op1),
	C =.. [Op|T],
	!,
	C1 =..[Op1|T],
	do(L,C1),
	doConditions(L,CC).
doConditions(_,[]).

doProgram(program(Domains, Conditions), L) :-
	findall(V,member(domain(V,_),Domains), VV),
	append(VV, Vars),
	makeVars(Vars, _, L),
	doDomains(L, Domains),
	doConditions(L, Conditions),
	do(L, label(Vars)).
	
atom_to_term(A, T) :-
	atom_concat(A,'.',A1),
	new(S, string(A1)),
	pce_open(S,read, In),
	read(In, T),	close(In).

program(program(Domains, Conditions)) -->
	[domains,:],
	domains(Domains),
	[conditions,:],
	conditions(Conditions).

domains([D | DD]) -->
	domain(D),
	domains(DD),
	!.
domains([])-->[].

domain(domain(Vars, Nums)) -->
	{Vars=[_|_],Nums=[_|_]},
	Vars,
	[from],
	Nums,
	[;].

conditions([C | CC]) -->
	condition(C),
	conditions(CC),
	!.
conditions([])-->[].

condition(C) -->
	{L=[_|_]},
	L,
	[;],
	{concat_atom(L,LA),
	 atom_to_term(LA,C)
	}.
       

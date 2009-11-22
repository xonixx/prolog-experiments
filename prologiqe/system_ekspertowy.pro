% definicje operatorow

:- op(1200,xfx,if).
:- op(1100,xfx,and).
:- op(1100,xfx,&).
:- op(900,fx,not).


:-dynamic(known/3).


%baze wiedzy bedziemy czerpali z tego pliku
:- consult('bazawiedzy.pro').


expert :- %wywoluje system ekspertowy
	writeln('*** System ekspertowy do diagnozowania aparatu fotograficznego ***'),
	writeln('Udzielaj odpowiedzi na zadane pytania wstawiajac yes/no.'),
	writeln('Kazda odpowiedz koncz kropka'),
	retractall(known(_,_,_)), %usuwa wszystkie informacje z bazy wiedzy
	solve(malfunctions(aparat), X), %rozpoczyna rozwiazywanie problemu
	print_proof(X).  %drukuje drzewo dowodu

%system rozumowania
solve(true, void).
solve(X and Y,Px & Py):-solve(X,Px),solve(Y,Py).
solve(not(X),proof(not(X),void)):-not(solve(X,P)).
solve(symptom(X,Y),proof(symptom(X,Y),void)):-confirm(X,Y).
solve(X, proof(X,Py)):-(X if Y ),solve(Y,Py).


%zczytywanie odpowiedzi uzytkownika
confirm(X,Y):-known(X,Y,true).
confirm(X,Y):-not(known(X,Y,Z)),nl,ask(X,Y),
	read(A),remember(X,Y,A),A = yes.

%pamietanie odpowiedzi uzytkownika
remember(X, Y, yes) :- 
	assertz(known(X, Y, true)).
remember(X, Y, no) :- 
	assertz(known(X, Y, false)).


%pytamy uzytkownika, aby znalezc problem
ask(brak_ostrosci_uszkodzenie_matrycy,ostrosc) :- 
	write('Czy aparat nie ustawia ostrosci?[yes/no]: ').
ask(zly_film_uszkodzenie_matrycy,film) :- 
	write('Czy film jest w zlej jakosci?[yes/no]: ').
ask(nie_dziala_wlacz_tryb_przegladania_zdjec,przegladanieZdjec) :- 
	write('Czy nie dziala przegladanie zdjec?[yes/no]: ').
ask(sa_uszkodzona_matryca,paski) :- 
	write('Czy na zdjeciach sa paski?[yes/no]: ').
ask(przywrocone_uszkodzona_matryca,ustawieniaFabryczne) :- 
	write('Czy przywrocone sa ustawienia fabryczne i problem wystepuje dalej?[yes/no]: ').

ask(zaMalaIlosc,iloscZdjec) :- 
	write('Czy ilosc wykonanych zdjec jest znacznie mniejsza od maksymalnej liczby zdjec mogacej zmiescic sie na karte?[yes/no]: ').
ask(pokazujeSie,komunikat) :- 
	write('Czy pojawia sie komunikat NO FREE SPACE?[yes/no]: ').

%drukuje drzewo dowodu
print_proof(void).
print_proof(X & Y ) :- 
	print_proof(X), 
	nl, 
	print_proof(Y).
print_proof(proof(X, void)) :- 
	write(X),
	nl.
print_proof(proof(X, Y )) :- 
	not(Y = void), 
	write(X), 
	write(' BECAUSE'), 
	nl, 
	print_children(Y), 
	nl, 
	print_proof(Y).


print_children(proof(X, Y) & Z) :- 
	write('      '), 
	write(X), 
	write(' AND'), 
	nl, 
	print_children(Z).
print_children(proof(X, Y )) :- 
	write('      '), 
	write(X),
	 nl.




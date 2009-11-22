
% :- dynamic auto/3.

doif(Condition, Action) :-
	(Condition, !, Action); true.
	
start :- 
	consult('db.pl'),
	menu.

wr(nl) :- nl, !.
wr(W) :- write(W), !.

w([H | T]) :-
	wr(H),w(T), !.
w([]).

menu :-
	w([nl,
	   '----------------------------------------', nl,
	   '|  ���� ������ "���������"             |', nl,
	   '|                                      |', nl,
	   '|  1 - �������� ����������� ����       |', nl,
	   '|  2 - ���������� ������               |', nl,
	   '|  3 - �������� ������                 |', nl,
	   '|  4 - �����                           |', nl,
	   '|  5 - ��������������                  |', nl,
	   '|  � - �����                           |', nl,
	   '----------------------------------------', nl
	   ]),
	write('������� ��� �����: '), 
	read(Choice), 
	do(Choice), !,
	doif(Choice \= �, % will fail on � -> menu will exit
	     menu).

write_auto_by_id(Id) :-
	auto(id(Id), M, P),
	write_auto(id(Id), M, P).

write_auto(Auto) :-
	Auto = auto(id(Id), model(Name, Model, Color, Year), price(Price)),
	write_auto(id(Id), model(Name, Model, Color, Year), price(Price)).

write_auto(id(Id), model(Name, Model, Color, Year), price(Price)) :-
	w([Id, ') �����=', Name, 
	   ', ������=', Model, 
	   ', ����=', Color,
	   ', ��� �������=', Year,
	   ', ����=', Price, ';',
	   nl
	  ]).

do(1) :-
	w([nl, '���������� � ����:', nl, nl]),
	auto(Id, M, P),
	write_auto(Id, M, P),
	fail.

do(1).

do(2) :-
	w([nl, '���������� ������:', nl]),
	write('�����: '), read(N),
	write('������: '), read(M),
	write('����: '), read(C),
	write('��� �������: '), read(Y),
	write('����: '), read(P),
	max_id(MaxId),
	Next is MaxId + 1,
	assertz(auto(id(Next), model(N, M, C, Y), price(P))),
	write('���������!').
	
do(3) :-
	w(['��������...', nl,
	  '������� # ��������� ������: '
	  ]),
	read(Id),
	remove_auto(Id).

do(�) :- write('��������� ����...'),nl,
	save_db,
	retract_all(auto(_, _, _)),
	write('�� ��������!').

do(4) :- search.

do(5) :- 
	w([nl,
	   '��������������', nl,
	  '������� # ������, ������� �� ������� �������������: ']),
	read(Id),
	edit_auto(Id).

do(Other) :-
	w(['�� ����� �������� �������: ', Other, '!']).

search :-
	w([nl, nl,
	    '-----------------------', nl,
	    '| �����:              |', nl,
	    '|                     |', nl,
	    '| 1 - �� ������       |', nl,
	    '| 2 - �� �����        |', nl,
	    '| 3 - �� ����         |', nl,
	    '| � - ����� � ������� |', nl,
	    '-----------------------', nl,
	    '������� ��� �����: '
	   ]),
	read(Choice),
	search(Choice), !,
	doif(Choice \= �, 
	     search).

search(1) :-
	w([nl,
	  '����� �� ������:', nl,
	  '������� ������: ']),
	read(Model),
	search_model(Model).

search(2) :-
	w([nl,
	  '����� �� �����:', nl,
	  '������� ����: ']),
	read(Color),
	search_color(Color).       

search(3) :-
	w([nl,
	  '����� �� ����:', nl, 
	   '����� ������� ���������� �� ������ ��������� ����: ']),
	read(Price),
	search_price(Price).

search(�) :- write('������������...').

search_model(Name) :-
	write('�������:'), nl,
	auto(Id, model(Name, M, C, Y), Price),
	write_auto(Id, model(Name, M, C, Y), Price),
	fail.
search_model(_).

search_color(Color) :-
	write('�������:'), nl,
	auto(Id, model(N, M, Color, Y), Price),
	write_auto(Id, model(N, M, Color, Y), Price),
	fail.
search_color(_).


search_price(Price) :-
	write('�������:'), nl,
	auto(Id, Model, price(P)),
	P =< Price,
	write_auto(Id, Model, price(P)),
	fail.
search_price(_).	

get_auto(Id, auto(id(Id), M, P)) :-
	auto(id(Id), M, P).

remove_auto(�) :-!.
remove_auto(Id) :-
	retract(auto(id(Id), _, _)),
	w(['������ #', Id, ' ������� �������.', nl]).

remove_auto(Id) :-
	w(['������ � #', Id, ' �� ����������!', nl,
	  '��������� ������� ��� ������� � ��� �������� � ����: ']),
	read(NewId),
	remove_auto(NewId).

edit_auto(�) :-!.
edit_auto(Id) :-
	get_auto(Id, Auto),
	write_auto(Auto),
	w([nl,
	   '--------------------', nl,
	   '| �������������:   |', nl,
	   '|                  |', nl,
	   '| 1 - ����         |', nl,
	   '| 2 - ����         |', nl,
	   '--------------------', nl,
	   '������� ��� �����: '
	  ]),
	read(Choice),
	do_edit_auto(Choice, Auto),
	write('����������������: '), nl,
	write_auto_by_id(Id).
	
edit_auto(Id) :-
	w(['������ � #', Id, ' �� ����������!', nl,
	  '��������� ������� ��� ������� � ��� �������� � ����: ']),
	read(NewId),
	edit_auto(NewId).

do_edit_auto(1, auto(id(Id), model(Name, Model, Color, Year), _)) :-
	write('������� ����� ����: '),
	read(NewPrice),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, Color, Year), price(NewPrice))).

do_edit_auto(2, auto(id(Id), model(Name, Model, _, Year), price(Price))) :-
	write('������� ����� ����: '),
	read(NewColor),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, NewColor, Year), price(Price))).

do_edit_auto(_, _) :-
	write('�� ������ �����!').

save_db :-
	tell('db.pl'),
	listing(auto),
	told.

% -----------------------------------

retract_all(Fact):- retract(Fact),
                   fail.
retract_all(_).

% -----------------------------------

id(Id) :-
	auto(id(Id), _, _).
	

set_max(N) :-
	retract(max_id_(_)),
	assert(max_id_(N)).

set_max(N) :-
	assert(max_id_(N)).

max_id(Max) :-
	set_max(0),
	define_max,
	max_id_(Max).

define_max :-
	id(Id),
	max_id_(C),
	Id > C,
	set_max(Id),
	fail.
define_max.

	








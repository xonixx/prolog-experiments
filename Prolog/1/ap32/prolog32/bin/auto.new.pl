
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
	   '|  База знаний "Автосалон"             |', nl,
	   '|                                      |', nl,
	   '|  1 - просмотр содержимого базы       |', nl,
	   '|  2 - добавление записи               |', nl,
	   '|  3 - удаление записи                 |', nl,
	   '|  4 - поиск                           |', nl,
	   '|  5 - редактирование                  |', nl,
	   '|  в - выход                           |', nl,
	   '----------------------------------------', nl
	   ]),
	write('Введите ваш выбор: '), 
	read(Choice), 
	do(Choice), !,
	doif(Choice \= в, % will fail on в -> menu will exit
	     menu).

write_auto_by_id(Id) :-
	auto(id(Id), M, P),
	write_auto(id(Id), M, P).

write_auto(Auto) :-
	Auto = auto(id(Id), model(Name, Model, Color, Year), price(Price)),
	write_auto(id(Id), model(Name, Model, Color, Year), price(Price)).

write_auto(id(Id), model(Name, Model, Color, Year), price(Price)) :-
	w([Id, ') марка=', Name, 
	   ', модель=', Model, 
	   ', цвет=', Color,
	   ', год выпуска=', Year,
	   ', цена=', Price, ';',
	   nl
	  ]).

do(1) :-
	w([nl, 'Автомобили в базе:', nl, nl]),
	auto(Id, M, P),
	write_auto(Id, M, P),
	fail.

do(1).

do(2) :-
	w([nl, 'Добавление записи:', nl]),
	write('Марка: '), read(N),
	write('Модель: '), read(M),
	write('Цвет: '), read(C),
	write('Год выпуска: '), read(Y),
	write('Цена: '), read(P),
	max_id(MaxId),
	Next is MaxId + 1,
	assertz(auto(id(Next), model(N, M, C, Y), price(P))),
	write('Добавлено!').
	
do(3) :-
	w(['Удаление...', nl,
	  'Введите # удаляемой машины: '
	  ]),
	read(Id),
	remove_auto(Id).

do(в) :- write('Сохраняем базу...'),nl,
	save_db,
	retract_all(auto(_, _, _)),
	write('До свидания!').

do(4) :- search.

do(5) :- 
	w([nl,
	   'Редактирование', nl,
	  'Введите # машины, которую вы желаете редактировать: ']),
	read(Id),
	edit_auto(Id).

do(Other) :-
	w(['Вы ввели неверный вариант: ', Other, '!']).

search :-
	w([nl, nl,
	    '-----------------------', nl,
	    '| Поиск:              |', nl,
	    '|                     |', nl,
	    '| 1 - по модели       |', nl,
	    '| 2 - по цвету        |', nl,
	    '| 3 - по цене         |', nl,
	    '| в - выход в главное |', nl,
	    '-----------------------', nl,
	    'Введите ваш выбор: '
	   ]),
	read(Choice),
	search(Choice), !,
	doif(Choice \= в, 
	     search).

search(1) :-
	w([nl,
	  'Поиск по модели:', nl,
	  'Введите модель: ']),
	read(Model),
	search_model(Model).

search(2) :-
	w([nl,
	  'Поиск по цвету:', nl,
	  'Введите цвет: ']),
	read(Color),
	search_color(Color).       

search(3) :-
	w([nl,
	  'Поиск по цене:', nl, 
	   'Будут найдены автомобили не дороже указанной цены: ']),
	read(Price),
	search_price(Price).

search(в) :- write('Возвращаемся...').

search_model(Name) :-
	write('Найдено:'), nl,
	auto(Id, model(Name, M, C, Y), Price),
	write_auto(Id, model(Name, M, C, Y), Price),
	fail.
search_model(_).

search_color(Color) :-
	write('Найдено:'), nl,
	auto(Id, model(N, M, Color, Y), Price),
	write_auto(Id, model(N, M, Color, Y), Price),
	fail.
search_color(_).


search_price(Price) :-
	write('Найдено:'), nl,
	auto(Id, Model, price(P)),
	P =< Price,
	write_auto(Id, Model, price(P)),
	fail.
search_price(_).	

get_auto(Id, auto(id(Id), M, P)) :-
	auto(id(Id), M, P).

remove_auto(в) :-!.
remove_auto(Id) :-
	retract(auto(id(Id), _, _)),
	w(['Машина #', Id, ' успешно удалена.', nl]).

remove_auto(Id) :-
	w(['Машина с #', Id, ' не существует!', nl,
	  'Повторите попытку или введите в для возврата в меню: ']),
	read(NewId),
	remove_auto(NewId).

edit_auto(в) :-!.
edit_auto(Id) :-
	get_auto(Id, Auto),
	write_auto(Auto),
	w([nl,
	   '--------------------', nl,
	   '| Редактировать:   |', nl,
	   '|                  |', nl,
	   '| 1 - цену         |', nl,
	   '| 2 - цвет         |', nl,
	   '--------------------', nl,
	   'Введите ваш выбор: '
	  ]),
	read(Choice),
	do_edit_auto(Choice, Auto),
	write('Отредактированно: '), nl,
	write_auto_by_id(Id).
	
edit_auto(Id) :-
	w(['Машина с #', Id, ' не существует!', nl,
	  'Повторите попытку или введите в для возврата в меню: ']),
	read(NewId),
	edit_auto(NewId).

do_edit_auto(1, auto(id(Id), model(Name, Model, Color, Year), _)) :-
	write('Введите новую цену: '),
	read(NewPrice),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, Color, Year), price(NewPrice))).

do_edit_auto(2, auto(id(Id), model(Name, Model, _, Year), price(Price))) :-
	write('Введите новый цвет: '),
	read(NewColor),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, NewColor, Year), price(Price))).

do_edit_auto(_, _) :-
	write('Не верный выбор!').

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

	








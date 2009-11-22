
% do Action only if Condition, succeeds in any case
doif(Condition, Action) :-
	(Condition, !, Action); true.

% call this to start program
start :- 
	reconsult('db.pl'),
	menu.

% helpful write predicates
wr(nl) :- nl, !.
wr(W) :- write(W), !.

w([H | T]) :-
	wr(H),w(T), !.
w([]).

% menu
menu :-
	w([nl,
	   '----------------------------------------', nl,
	   '|  Knowledge base "Autos"              |', nl,
	   '|                                      |', nl,
	   '|  1 - view db content                 |', nl,
	   '|  2 - add auto                        |', nl,
	   '|  3 - remove auto                     |', nl,
	   '|  4 - search                          |', nl,
	   '|  5 - edit                            |', nl,
	   '|  e - exit                            |', nl,
	   '----------------------------------------', nl
	   ]),
	write('Enter your choice: '), 
	read(Choice), 
	do(Choice), !,
	doif(Choice \= e, % will fail on e -> menu will exit
	     menu).

% predicates to render auto
write_auto_by_id(Id) :-
	auto(id(Id), M, P),
	write_auto(id(Id), M, P).

write_auto(Auto) :-
	Auto = auto(id(Id), model(Name, Model, Color, Year), price(Price)),
	write_auto(id(Id), model(Name, Model, Color, Year), price(Price)).

write_auto(id(Id), model(Name, Model, Color, Year), price(Price)) :-
	w([Id, ') brand=', Name, 
	   ', model=', Model, 
	   ', color=', Color,
	   ', year=', Year,
	   ', price=', Price, ';',
	   nl
	  ]).

% process menu choices
do(1) :-
	w([nl, 'All cars in db:', nl, nl]),
	auto(Id, M, P),
	write_auto(Id, M, P),
	fail.

do(1).

do(2) :-
	w([nl, 'Adding record:', nl]),
	write('Brand: '), read(N),
	write('Model: '), read(M),
	write('Color: '), read(C),
	write('Year: '), read(Y),
	write('Price: '), read(P),
	max_id(MaxId),
	Next is MaxId + 1,
	assertz(auto(id(Next), model(N, M, C, Y), price(P))),
	write('Successfully added!').
	
do(3) :-
	w(['Removing...', nl,
	  'Enter the # of car you want to remove or ''e'' for exit: '
	  ]),
	read(Id),
	remove_auto(Id).

do(e) :- write('Saving db... '),
	save_db,
	retract_all(auto(_, _, _)),
	write('Saved.'), nl,
	write('Bye!').

do(4) :- search.

do(5) :- 
	w([nl,
	   'Editing...', nl,
	  'Enter the # of car you want to edit: ']),
	read(Id),
	edit_auto(Id).

do(Other) :-
	w(['You entered wrong choice: ', Other, '!']).

% searching predicates
search :-
	w([nl, nl,
	    '-----------------------', nl,
	    '| Search:             |', nl,
	    '|                     |', nl,
	    '| 1 - by brand        |', nl,
	    '| 2 - by color        |', nl,
	    '| 3 - by price        |', nl,
	    '| e - return to main  |', nl,
	    '-----------------------', nl,
	    'Enter your choice: '
	   ]),
	read(Choice),
	search(Choice), !,
	doif(Choice \= e, 
	     search).

search(1) :-
	w([nl,
	  'Search by brand:', nl,
	  'Enter brand: ']),
	read(Model),
	search_model(Model).

search(2) :-
	w([nl,
	  'Search by color:', nl,
	  'Enter color: ']),
	read(Color),
	search_color(Color).       

search(3) :-
	w([nl,
	  'Search by price:', nl, 
	  'Search for cars cheaper then this price: ']),
	read(Price),
	search_price(Price).

search(e) :- write('Returning...').

search_model(Name) :-
	write('Found:'), nl,
	auto(Id, model(Name, M, C, Y), Price),
	write_auto(Id, model(Name, M, C, Y), Price),
	fail.
search_model(_).

search_color(Color) :-
	write('Found:'), nl,
	auto(Id, model(N, M, Color, Y), Price),
	write_auto(Id, model(N, M, Color, Y), Price),
	fail.
search_color(_).


search_price(Price) :-
	write('Found:'), nl,
	auto(Id, Model, price(P)),
	P =< Price,
	write_auto(Id, Model, price(P)),
	fail.
search_price(_).	

get_auto(Id, auto(id(Id), M, P)) :-
	auto(id(Id), M, P).

% removing predicates
remove_auto(e) :-!.
remove_auto(Id) :-
	retract(auto(id(Id), _, _)),
	w(['Car #', Id, ' was successfully removed.', nl]).

remove_auto(Id) :-
	w(['Car with #', Id, ' doesn''t exist!', nl,
	  'Retry or enter ''e'' for return to menu: ']),
	read(NewId),
	remove_auto(NewId).

% editing predicates
edit_auto(e) :-!.
edit_auto(Id) :-
	get_auto(Id, Auto),
	write_auto(Auto),
	w([nl,
	   '--------------------', nl,
	   '| Edit:            |', nl,
	   '|                  |', nl,
	   '| 1 - price        |', nl,
	   '| 2 - color        |', nl,
	   '--------------------', nl,
	   'Enter your choice: '
	  ]),
	read(Choice),
	do_edit_auto(Choice, Auto),
	write('Edited: '), nl,
	write_auto_by_id(Id).
	
edit_auto(Id) :-
	w(['Car with #', Id, ' doesn''t exist!', nl,
	  'Retry or enter ''e'' for return to menu: ']),
	read(NewId),
	edit_auto(NewId).

do_edit_auto(1, auto(id(Id), model(Name, Model, Color, Year), _)) :-
	write('Enter new price: '),
	read(NewPrice),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, Color, Year), price(NewPrice))).

do_edit_auto(2, auto(id(Id), model(Name, Model, _, Year), price(Price))) :-
	write('Enter new color: '),
	read(NewColor),
	retract(auto(id(Id), _, _)),
	assert(auto(id(Id), model(Name, Model, NewColor, Year), price(Price))).

do_edit_auto(_, _) :-
	write('Wrong choice!').

% saves all autos as prolog file of facts
save_db :-
	tell('db.pl'),
	listing(auto),
	told.

% retractall
retract_all(Fact):- retract(Fact),
                   fail.
retract_all(_).

% predicates to define max id of car in db to generate next id
id(Id) :-
	auto(id(Id), _, _).

set_max(N) :-
	retract_all(max_id_(_)),
	assert(max_id_(N)), !.

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

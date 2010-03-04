

start :-
	new(D, dialog('Сумма двух чисел')),
	send_list(D, append,
		  [
		   new(T1, text_item('Первое число')),
		   new(T2, text_item('Второе число')),
		   label('Сумма', 'Сумма:'),
		   new(Ts, label('Сумма', 'qqq')),
		   button('Сложить',
			  message(@prolog,
				  do_add,
				  T1,T2,Ts)),
		   button('Выход',
			  message(D, destroy))
		  ]),
	send(D, center, Ts?right),
	send(D, open).

do_add(T1, T2, Ts) :-
	get(T1, selection, Txt1),
	get(T2, selection, Txt2),
	atom_number(Txt1, N1),
	atom_number(Txt2, N2),
	N3 is N1 + N2,
	write(N3),
	send(Ts, selection, N3).


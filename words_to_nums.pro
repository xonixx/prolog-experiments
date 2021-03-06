
число(Ч) --> 
	тысячи(Т),
	сотни(С),
	{
	 Ч is Т + С
	},
	!.
число(Ч) --> сотни(Ч).

тысячи(Т) -->
	единицы(Е),
	тысячи,
	{
	 Т is Е * 1000
	}.

тысячи --> [тысяча].
тысячи --> [тысячи].
тысячи --> [тысяч].

сотни(С) -->
	сотня(С1),
	десятки(Д),
	{
	 С is С1 * 100 + Д
	},
	!.
%сотни(С) --> сотня(С), !.
сотни(С) --> десятки(С).

десятки(Д) -->
	десяток(Д0),
	единицы(Е),
	{
	 Д is Д0 * 10 + Е
	},
	!.
десятки(11) --> [одинадцать].
десятки(12) --> [двенадцать].
десятки(13) --> [тринадцать].
десятки(14) --> [четырнадцать].
десятки(15) --> [пятнадцать].
десятки(16) --> [шестнадцать].
десятки(17) --> [семнадцать].
десятки(18) --> [восемнадцать].
десятки(19) --> [девятнадцать].

%десятки(Д) --> десяток(Д0), {Д is }
десятки(Д) --> единицы(Д).

десяток(2) --> [двадцать].
десяток(3) --> [тридцать].
десяток(4) --> [сорок].
десяток(5) --> [пятьдесят].
десяток(6) --> [шестьдесят].
десяток(7) --> [семьдесят].
десяток(8) --> [восемьдесят].
десяток(9) --> [девяносто].
%десяток(0) --> [].


сотня(1) --> [сто].
сотня(2) --> [двести].
сотня(3) --> [триста].
сотня(4) --> [четыреста].
сотня(5) --> [пятьсот].
сотня(6) --> [шестьсот].
сотня(7) --> [семьсот].
сотня(8) --> [восемьсот].
сотня(9) --> [девятьсот].



единицы(1) --> [один].
единицы(1) --> [одна].

единицы(2) --> [два].
единицы(2) --> [две].

единицы(3) --> [три].
единицы(4) --> [четыре].
единицы(5) --> [пять].
единицы(6) --> [шесть].
единицы(7) --> [семь].
единицы(8) --> [восемь].
единицы(9) --> [девять].
единицы(0) --> [].

lex([H|T]) -->
	wrd(H),
	" ",
	!,
	lex(T).

lex([H]) --> wrd(H).
	
wrd(A) --> {W=[_]; W=[_|_]}, W, {string_to_atom(W, A)}.

строка_в_число(Стр, Числ) :-
	phrase(lex(L), Стр),
	phrase(число(Числ), L),
	!.


% набор из 4 фактов, представляющих отношение "мальчик", применимое к одному параметру (имени мальчика)
boy(tom).
boy(pit).
boy(buzz).
boy(john).

% --//-- отношение "девочка" --//--
girl(ann).
girl(jill).
girl(kate).
girl(emily).

% набор из фактов, выражающих отношение "любит" между двумя субъектами
% (в смысле первый любит второго)
loves(tom, jill).
loves(jill, buzz).
loves(jill, ann).
loves(pit, ann).
loves(ann, pit).
loves(buzz, pit).
loves(kate, kate).



in_love(X) :-
	loves(X, _). % X в кого-то влюблен

is_loved(X) :-
	loves(_, X). % кто-то влюблен в X

% взаимная любовь
mutual_love(X) :-
	loves(X, Y), % некто любит кого-то
	loves(Y, X). % а этот кто-то любит его(её).

% безответная любовь
answerless_love(X) :-
	loves(X, Y), % --//--
	\+ loves(Y, X). % a этот кто-то - нет.

% ориентация =)
orient(X, gomo) :-
	(   boy(X), loves(X, Y), boy(Y) % мальчик любит мальчика или
	;   girl(X), loves(X, Y), girl(Y) % девочка любит девочку
	), X \= Y. % и это не один и тот-же человек
	
orient(X, getero) :-
	(   boy(X), loves(X, Y), girl(Y) % мальчик любит девочку или
	;   girl(X), loves(X, Y), boy(Y) % наоборот
	), X \= Y. % --//--

orient(X, bi) :-
	orient(X, gomo), 
	orient(X, getero).

orient(X, narciss) :- 
	loves(X, X). % любит сам себя Oo










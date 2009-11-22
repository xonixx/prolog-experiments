
human(platon).
human(sokrat).

dog(tuzik).
dog(sharik).

cat(tom).

mouse(jerry).

animal(X) :-
	dog(X);
	cat(X);
	mouse(X).

age(platon, 80).
age(sokrat, 70).
age(tuzik, 10).
age(sharik, 7).
age(tom, 5).
age(jerry, 3).

mortal(Someone) :- 
	human(Someone);
	animal(Someone).

film(1, "Abirvalg 123", "comedy", 1995).
film(2, "Dojit do pensii", "drama", 2000).
film(3, "Dojit do utra", "triller", 2007).

contains(Str, Substr) :-
  string_to_atom(Str, AStr),
  string_to_atom(Substr, ASubstr),
  string_concat(_, S2, AStr),
  (string_concat(_, ASubstr, S2); string_concat(ASubstr, _, S2)).
  
p(Str) :-
  string_to_atom(Str, A),
  write(A).

write_film(Id) :-
  film(Id, Name, Type, Year),
  p("Фильм("),p(Id),p(,),
  p(Name),p(,),
  p(Type),p(,),
  p(Year),p(")"),nl.
  
by_part_of_name(PartOfName) :-
  film(Id, Name, _, _),
  (contains(Name, PartOfName) -> write_film(Id),
  fail).
  
  
dog(pit).
dog(joe).
dog(sem).

t :-
  dog(X),
  (atom(X) -> (write(X),nl),
   fail).



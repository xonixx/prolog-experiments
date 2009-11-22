sum([H1,H2 | T],R):-!, H is H1+H2,sum([H | T],R).
sum([H],H).

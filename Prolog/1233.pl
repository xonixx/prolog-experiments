

isL(List1, List2):-
  append(F, L, List2),
  write(F),write(+),write(L),nl,
  append(F, L1, List1),
  (append(L, [_, _], L1);append([_,_], L, L1)).
  


% проверяет, является ли List1 списком List с удаленным 1 произв. эл-том
remove1(List, List1) :-
  append(F, L, List),
  append(F, L1, List1),
  (append(L1, [_], L); append([_], L1, L)).

isN3Range1([], _).
isN3Range1([H | T], N) :-
  is(H, N^3),
  is(N1, N+1),
  isN3Range1(T, N1).

% проверяет, является ли список рядом из членов n^3
isN3Range([]). % считаем, что пустой список является
isN3Range(L) :-
  isN3Range1(L, 1).

% собственно решение задачи
test1(List) :-
  remove1(List, List1),
  write(List1),nl,
  isN3Range(List1).

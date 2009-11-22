
:- module(xlists,
 [first/2,
  filter/4,
  all/2,
  any/2]).

%
% first
%

first([A | _], A) :-!.

% last

%
% filter/4
%

filter([], _, [], []) :-!.

filter([H], Predicate, [H], []) :-
 apply(Predicate, [H]),
 !.
 
filter([H], Predicate, [], [H]) :-
 \+ apply(Predicate, [H]),
 !.

filter([H | T], Predicate, Lyes, Lno) :-
 (apply(Predicate, [H]) -> (Lyes = [H | Tyes],
                         filter(T, Predicate, Tyes, Lno));
                         
                         (Lno = [H | Tno],
                         filter(T, Predicate, Lyes, Tno))).
                         
%
% all
%

all([], _) :-!.

all([H | T], Predicate) :-
 apply(Predicate, [H]),
 all(T, Predicate).
 
%
% any
%

any([], _) :-
 fail,
 !.
 
any([H | T], Predicate) :-
 apply(Predicate, [H]); any(T, Predicate).
 
%
% get_key_value
%

is_null(Val) :-
 Val = null.

get_key_value(Hash, Key, Val) :-
 member(Key/Val, Hash), !
 ; is_null(Val).
 
%
% min_el
%

min_el([El], El) :-!.
min_el([H | T], Res) :-
 min_el(T, M1),
 Res is min(H, M1).
 
%
% max_el
%

max_el([El], El) :-!.
max_el([H | T], Res) :-
 max_el(T, M1),
 Res is max(H, M1).



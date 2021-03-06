% ? ??????? ?????? ? ??????????? ????? ??????: ??????????? ????, ??????, ?????, ????????, ??????????, ??????????.
% ?????? ?? ????? ?????????????? ? ???????, ????????, ????????, ???????, ????????, ??????? ? ?????? ???????? ???? ????. ???????? ?????????.
%
% ???? ???????? ?? ????????? ?????, ?? ?? ????? ????????; ?? ?? ????? ?? ??????? ????? ? ??????? ??????.
% ??????? ?? ?????????, ?? ?????? ????? ?????????????? ?? ?????. ??????? ? ???????? ????????? ??????????? ???? ??? ??????.
% ?? ????????? ????? ? ?????, ?????? ??? ??????????? ????, ?? ????????????? ?? ??????? ? ?? ????????.
% ????? ???? ???????? ???? ?? ????????, ?? ???????. ?? ?????? ????? ??????????? ????, ????? ??? ??????????: ????????????? ? ???????? ??? ????????.
% ?????????, ?????? (???????? ?????? ????????!) ?????? ????? ?????????????, ?????? ???? ??? ???????? ????.
% ????????????? ??????? ????????????? ?? ????? ??????? ?????.
%
% ????????? ?????????? ?? ??????????? ??? 9-?? ??????, ??????????????? ???? ????????????? ???????????, ? ????: ????-???????-???????.

:- op(800, xfy, =>).

If => Then :- If -> Then; true.

check(N, L, P) :-
	P = s => (member(L, [chem, biol]), N \= 3),
	P = f => (L \= lit, N \= 2),
	member(P, [a, v]) => member(L, [eng, phys]),
	N = 4 => (member(L, [chem, phys, eng]), \+ member(P, [a, v])),
	N = 5 => member(P, [v, d]),
	N = 6 => (member(L, [eng, chem, lit]), member(P, [v, s])),
	L = math => N mod 2 =:= 0,
	L = biol => N > 2.

check_all([L | LL1], PP, RR) :-
	select(N-L-P, RR, RR1),
	select(P, PP, PP1),
	check(N, L, P),
	check_all(LL1, PP1, RR1).
check_all([], [], []).

solve(Res) :-
	LL = [eng, phys, chem, biol, lit, math],
	PP = [a, v, s, d, e, f],
	Res = [1-_-_, 2-_-_, 3-_-_, 4-_-_, 5-_-_, 6-_-_],
	check_all(LL, PP, Res).


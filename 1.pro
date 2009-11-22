

doit :-
	process([aaa, bbb, abc, zzz, qqq, qwerty, xyz, zzz]).

process([H | T]) :-
	atom_codes(H, S),
	S = [F | _],
	atom_codes(Fa, [F]),
	append(Fa),
	format('~w~n', [H]),
	told,
	process(T).

process([]).

%%%

doit1 :-
	f([
	   dir1/file1,
	   dir1/dir2/file2,
	   dir1/dir3/file3,
	   dir4/file4
	  ], dir1, Res),
	write(Res).

f([D/D1/_ | T], D, (Fres, [D1 | Dres])) :-
	f(T, D, (Fres, Dres)), !.

f([D/F | T], D, ([F | Fres], Dres)) :-
	f(T, D, (Fres, Dres)), !.

f([_ | T], D, (Fres, Dres)) :-
	f(T, D, (Fres, Dres)), !.

f([], _, ([], [])).

	


% File sampleproj.pl - M. Covington - 2001 April 21
% A tokenizer using DCG rules.
%:- use_module(library(lists)). % provides append/3 in SICStus Prolog

:- module(tokenizer, [
		      token_list/3%,
		      %tokenizer_opts_add/1
		     ]).
:- dynamic [tokenizer_opt/1].
	
% A test predicate to demonstrate that it works
test :- token_list(What,
		   " We owe\n $1,048,576.24 to Agent 007 for Version 3.14159! ",[]),
	write(What), nl,
	write('[There should not be any alternatives here...]'), nl,
	fail.

%tokenizer_opts_add(OptsList) :-
%	forall(member(Opt, OptsList), assert(tokenizer_opt(Opt))).

% A token list is a series of zero or more tokens.
% Its argument consists of the list of tokens, as atoms and numbers.
% The cut ensures that the maximum number of characters is
% gathered into each token.
%
% To tokenize a string, do this: ?- token_list(Result," the string ",[]).
%
token_list([T|Rest]) --> blank0,token(T),!,token_list(Rest).
token_list([]) --> blank0.

% blank0 is a series of zero or more blanks.
blank0 --> [C],{char_type(C,cntrl);char_type(C,white)},!,blank0.
blank0 --> [].

% Several kinds of tokens.
% This is where lists of characters get converted into atoms or numbers.
token(T) --> special(L), {atom_codes(T,L)}.
token(T) --> word(W), {atom_codes(T,W)}.
token(T) --> numeral(N), {number_codes(T,N)}.

% A word is a series of one or more letters.
% The rules are ordered so that we first try to gather as many
% characters into one digit_string as possible.
word([L|Rest]) --> letter(L),word(Rest).
word([L]) --> letter(L).

% A numeral is a list of characters that constitute a number.
% The argument of numeral(...) is the list of character codes.
numeral([C1,C2,C3|N]) --> ",", digit(C1), digit(C2), digit(C3), numeral(N).
numeral([C1,C2,C3]) --> ",", digit(C1), digit(C2), digit(C3).
numeral([C|N]) --> digit(C), numeral(N). % multiple digits
numeral([C]) --> digit(C). % single digit
numeral(N) --> decimal_part(N). % decimal point and more digits
decimal_part([46|Rest]) --> ".", digit_string(Rest).
digit_string([D|N]) --> digit(D),digit_string(N).
digit_string([D]) --> digit(D).

% Various kinds of characters...
digit(C) --> [C], {char_type(C,digit)}.
special([C]) --> [C], {char_type(C,punct)}.
letter(C) --> [C], {char_type(C,lower)}.
letter(C) --> [U], {char_type(U,upper), C is U+32}.
% Conversion to lowercase

% End of sampleproj.pl


'BRACKET_OPEN' --> "{".
'BRACKET_CLOSE' --> "}".
'EQUAL' --> "=".
'END' --> ";".
'COMMA' --> ",".

'SPACE_1' --> " ".
'SPACE_1' --> "\n".

'SPACE' --> 'SPACE_1', !.
'SPACE' --> 'SPACE_1', 'SPACE'.

'POSIBLE_SPACE' --> 'SPACE', !.
'POSIBLE_SPACE' --> "".

'POSIBLE_END' --> 'END'.
'POSIBLE_END' --> "".

acc_modif(acess(M)) --> 
	Mod,
	'SPACE',
	{(Mod="public"; Mod="private"; Mod="protected"), string_to_atom(Mod, M)}, 
	!.
acc_modif(acess(package)) --> 
	'POSIBLE_SPACE'.

class(class(className(ClassName), St, Fi, Ac)) --> 
	acc_modif(Ac),
	static(St), 
	final(Fi), 
	"class", 
	identif(ClassName).

full_class_def(class(CN, St, Fi, Acc, Ext, Impl)) --> 
	class(class(CN, St, Fi, Acc)), 
	ext(Ext), 
	impl(Impl).

static(static(yes)) --> "static", 'SPACE', !.
static(static(no)) --> "", 'POSIBLE_SPACE'.

final(final(yes)) --> "final", 'SPACE', !.
final(final(no)) --> "", 'POSIBLE_SPACE'.

ext(extends([Id])) --> "extends", 'SPACE', identif(Id), !.
ext(extends([])) --> "", 'POSIBLE_SPACE'.

impl(implements(Ids)) --> "implements", identifs(Ids), !.
impl(implements([])) --> "".
	
identif(Id) --> IdS, {string_to_atom(IdS, Id)}.

identifs([H | T]) --> identif(H), 'POSIBLE_SPACE', 'COMMA', 'POSIBLE_SPACE', identifs(T), !.
identifs([H]) --> identif(H), !.
identifs([]) --> "".


full_class(class(CN, St, Fi, Acc, Ext, Impl, Membs, Meths)) --> 
	full_class_def(class(CN, St, Fi, Acc, Ext, Impl)), 
	'POSIBLE_SPACE',
	'BRACKET_OPEN',
	'POSIBLE_SPACE',
	members_methods(Membs, Meths), 
	'BRACKET_CLOSE'.

members_methods([Mb1 | MbN], Meth) -->
	class_member(Mb1),
	members_methods(MbN, Meth), !.

members_methods(Memb, [Mth1 | MthN]) -->
	class_method(Mth1),
	members_methods(Memb, MthN), !.

members_methods([], []) --> [].

type_of(type(T)) --> TS, {string_to_atom(TS, T)}.

% tmp
expression(expr(E)) --> [E].

default_val(default(Val)) --> 'EQUAL', expression(Val), !.
default_val(default(no)) --> "".

class_member(member(Name, St, Fi, Ac, Type, Def)) -->
	acc_modif(Ac), 
	static(St),
	final(Fi),
	type_of(Type),
	identif(Name),
	default_val(Def),
	'END'.

statement_equal(equal(Mod, Type, Name, Val)) -->
	acc_modif(Mod),
	type_of(Type),
	identif(Name),
	'EQUAL',
	expression(Val).

statement(St) --> statement_equal(St), 'END'.

statements(statements([])) --> [].
statements(statements([S1 | SN])) -->
	statement(S1),
	statements(SN).

class_method(method(Name, St, Fi, Ac, Type, Statements)) -->
	acc_modif(Ac),
	static(St),
	final(Fi),
	type_of(Type),
	identif(Name),
	'BRACKET_OPEN',
	statements(Statements),
	'BRACKET_CLOSE'.


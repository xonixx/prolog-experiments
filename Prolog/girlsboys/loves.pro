
% ����� �� 4 ������, �������������� ��������� "�������", ���������� � ������ ��������� (����� ��������)
boy(tom).
boy(pit).
boy(buzz).
boy(john).

% --//-- ��������� "�������" --//--
girl(ann).
girl(jill).
girl(kate).
girl(emily).

% ����� �� ������, ���������� ��������� "�����" ����� ����� ����������
% (� ������ ������ ����� �������)
loves(tom, jill).
loves(jill, buzz).
loves(jill, ann).
loves(pit, ann).
loves(ann, pit).
loves(buzz, pit).
loves(kate, kate).



in_love(X) :-
	loves(X, _). % X � ����-�� �������

is_loved(X) :-
	loves(_, X). % ���-�� ������� � X

% �������� ������
mutual_love(X) :-
	loves(X, Y), % ����� ����� ����-��
	loves(Y, X). % � ���� ���-�� ����� ���(�).

% ����������� ������
answerless_love(X) :-
	loves(X, Y), % --//--
	\+ loves(Y, X). % a ���� ���-�� - ���.

% ���������� =)
orient(X, gomo) :-
	(   boy(X), loves(X, Y), boy(Y) % ������� ����� �������� ���
	;   girl(X), loves(X, Y), girl(Y) % ������� ����� �������
	), X \= Y. % � ��� �� ���� � ���-�� �������
	
orient(X, getero) :-
	(   boy(X), loves(X, Y), girl(Y) % ������� ����� ������� ���
	;   girl(X), loves(X, Y), boy(Y) % ��������
	), X \= Y. % --//--

orient(X, bi) :-
	orient(X, gomo), 
	orient(X, getero).

orient(X, narciss) :- 
	loves(X, X). % ����� ��� ���� Oo










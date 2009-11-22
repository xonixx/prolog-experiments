
% ������
dog(sharik). % �������� ��������, ��� ����� - ������
dog(tuzik). % --//--

% �����
cat(pushok).
cat(druzhok).

% �������
hamster(pit).

% �������
man(bill).
man(george).
man(barak).
man(platon).
man(sokrat).

% �������
woman(ann).
woman(kate).
woman(pam).

% ���� ��������
dead(sharik).
dead(platon).
dead(sokrat).

% �������
age(sharik, 18). % ������� ������ - 18 ���
age(tuzik, 10). % --//--
age(pushok, 5).
age(druzhok, 2).
age(pit, 1).
age(bill, 62).
age(george, 62).
age(barak, 47).
age(sokrat, 70).
age(platon, 80).
age(ann, 20).
age(kate, 25).
age(pam, 30).

% ��������
animal(X) :-
    dog(X); % ��� ���� ������
    cat(X); % ���� �����
    hamster(X). % ���� �������

% ����
human(X) :-
    man(X); % ���� �������
    woman(X). % ���� �������

% ����� (��� ������) ��������
living(X) :-
    animal(X);
    human(X).

% ����� (� ������ ������) ��������
alive(X) :-
    living(X),
    \+ dead(X).

% ������
old(X) :-
    (   animal(X)
    ->  age(X, Age),
        Age >= 10 % �������, ��� �������� ������ 10 ��� - ������
    ;   human(X),
        age(X, Age),
        Age >= 60 % �������, ��� ���� ������ 60 ��� - ������
    ),
    \+ dead(X). % ������, �� ��� ���� - �����

% ������� - ������ - ����� � �� ������
young(X) :-
    alive(X),
    \+ old(X).

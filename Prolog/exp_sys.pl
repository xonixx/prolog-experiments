
start :�
consult('animals.pl'),
              %  ��������� � ���� ����������
              %    �� ���� ������ */
       write('Think an animal, I''l try to guess it'),
       nl,
       animals, /* ������� �������� ���������� �������� */
       %retractall(_,dialog),
       /* ������� �������
          ���������� */
       %retractall(_,knowledge),
                  /* ������� ���������� �� ���������
          �������� � ��������� */
       nl,nl,write('Do you want play ones more? (1 - Yes, 2 - No)'),
       read_true_char(C),
       C='1',!,start.
start:�
       nl,nl,write('Bye!').
       %readchar(_).
animals:�
        rule(X,L),
        check_it(L),
        nl,write('I think this is'),write(X),
        nl,write('Am I right? (1 � yes, 2 � No)),
        read_true_char(C),C='1',!.
animals:�
        nl,write('I don''t know what is this animal'),nl,
        nl,write('Let us add it to the database?'),nl,
        update.
update:�
        nl,write('Enter animal''s name'),
        read(S),
        add_cond(L), /* ��������� �������� ��������� */
        assert(rule(S,L)), /* ��������� ����������
                  � ���� ������*/
        %save("animals.ddb",knowledge)
        /* ��������� ����
                   ������ � ���� */.
add_cond(L):�
             cond_is(_,'1'),!, /* ������� ����������
            � ��������� ��������� */
             nl,write('It is already known about this animal:'),
             print_cond(1,[],L1),
         /* ����� ��������� � ��������
            ���������� */
             nl,write('Is it known something else about the animal (1 � yes, 2 � no)'),
             read_true_char(C),
             read_cond(C,L1,L).
add_cond(L):�
             read_cond('1',[],L).
print_cond(H,L,L):�
        not(cond(H,_)),!,write('Nothing.'),nl.
print_cond(H,L,L1):�
         cond_is(H,'1'),!,
         cond(H,T),
         H1 is H+1,
         nl,write(T),
         print_cond(H1,[H | L],L1).
print_cond(H,L,L1):�
         H1 is H+1,
         print_cond(H1,L,L1).
read_cond('1',L,L2):�
          ex_cond(1,L,L1,N),
          new_cond(N,L1,L2),!.
read_cond(_,L,L):�!.
ex_cond(N,L,L,N):�
       not(cond(N,_)),!.
ex_cond(N,L,L1,N2):�
         cond_is(N,_),!,
         N1=N+1,
         ex_cond(N1,L,L1,N2).
ex_cond(N,L,L1,N2):�
         cond(N,S),
         nl,write('Is it '),write(S),write('? (1 � yes, 2 � no)'),
         read_true_char(A),
         wr_cond(A,N,L,L2),
         N1 is N+1,
         ex_cond(N1,L2,L1,N2).
wr_cond('1',N,L,[N | L]):�!.
wr_cond('2',_,L,L):�!.
new_cond(N,L,L1):�
       nl,write('Is there more properties? (1 � yes, 2� no)'),
       read_true_char(A),
       A='1',!,
       nl,write('Describe an animal''s property'),
       nl,write('as ''it <description of new property>'), readln(S),
       assertz(cond(N,S)),
                 /* ���������� ������ ��������
         � ���� ������ */
       N1 is N+1,
       new_cond(N1,[N | L],L1).
new_cond(_,L,L).
check_it([HT]):�
              test_cond(H),
              check_it(T).
check_it([]).
test_cond(H):-
              cond_is(H,'1'),!. /* � ���� ������� ����������
             � ������� �������� */
test_cond(H):�
              cond_is(H,'2'),!,
              fail. /* � ���� ������� ����������
           �� ���������� �������� */
test_cond(H):� /* � ���� ��� ������� ���������� � ������
       ��������, �������� �� � �������� */
              cond(H,S),
              nl,write('It '),write(S),write('? (1 � yes, 2 � no)'),
              read_true_char(A),
              assert(cond_is(H,A)),
              test_cond(H).
read_true_char(C):�
        get_char(C1),
        test(C1,C).
test(C,C):�
           '1'<=C,C<='2',!.
test(_,C):�
           write('Press 1 or 2!'),nl,
           get_char(C1),
           test(C1,C).

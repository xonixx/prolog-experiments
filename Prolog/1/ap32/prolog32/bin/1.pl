zap(v($���������$,$������$,$��������     $),y($17$,$07$,$1984$),t($72127$),a($�������� 18-95$)).
zap(v($������$,$���������$,$�������������$),y($08$,$04$,$1979$),t($43214$),a($����� 20-21$)).
zap(v($��������$,$�������$,$�������������$),y($15$,$02$,$1980$),t($48890$),a($���������� 12-44$)).
zap(v($������$,$���������$,$����������   $),y($10$,$10$,$1985$),t($35301$),a($������������� 3-38$)).
zap(v($�����$,$�����$,$���������         $),y($01$,$07$,$1983$),t($36049$),a($���������� 26-21$)).
zap(v($��������$,$������$,$������������  $),y($10$,$10$,$1985$),t($49930$),a($������������� 6-14$)).
zap(v($��������$,$�����$,$����������     $),y($08$,$11$,$1979$),t($43214$),a($����� 25-21$)).

start:-reconsult('fonspr.db'),
       menu.
start:-menu.       

menu:-cls,nl,
      write('                    ���� ������ "���������� ����������"'    ),nl,
      write('        	    1 - �������� ����������� ���� ������'        ),nl,
      write('        	    2 - ���������� ������'                       ),nl,
      write('        	    3 - �������� ������'                         ),nl,
      write('		    4 - �����'				         ),nl,
      write('		    5 - ��������� �������'                       ),nl,      
      write('		    6 - ��������� ������ ��������'               ),nl,      
      write('		    7 - ��������� ������ ����������'             ),nl,
      write('        	    8 - �����'                                   ),nl,
write('������� ��� �����: '),read_line(0,X),do(X).

do($1$):-nl,
write('        ���                    ���� ����.  �.��������       �����'),nl,         
    zap(v(F,I,O),y(D,M,G),t(N),a(H)),
    write(F),write(' '),write(I),write(' '),write(O),write('  '),
    write(D),write('.'),write(M),write('.'),write(G),write('  '),
    write('���.'),write(N),write('  '),write('��.'),
    write(H),write(' '),
    nl,fail.
do($1$):-get0_noecho(X),menu.

do($2$):-write('������� ��������� ������: '),nl,
         write('�������: '),read_line(0,F),
         write('���: '),read_line(0,I),
         write('��������: '),read_line(0,O),
         write('���� ��������: '),read_line(0,D),
         write('����� ��������: '),read_line(0,M),
         write('��� ��������: '),read_line(0,G),
         write('����� ��������: '),read_line(0,N),
         write('�������� ����� ����������: '),read_line(0,H),
         write('��������� ������: '),
         write(F),write(' '),write(I),write(' '),write(O),write('  '),
         write(D),write('.'),write(M),write('.'),write(G),write('  '),
         write('���.'),write(N),write('  '),write('��.'),
         write(H),write(' '),
         assertz(zap(v(F,I,O),y(D,M,G),t(N),a(H))),menu.

do($3$):-write('������� �������: '),read_line(0,F),
         retract(zap(v(F,_,_),y(_,_,_),t(_),a(_))),menu.
do($3$):-write('����� ������� ���'),get0_noecho(X),menu.       

do($4$):-cls,nl,
         write('              1 - ����� �� �������'),nl,
         write('              2 - ����� �� ����� � ��������'),nl,
         write('              3 - ����� �� ���� ��������'),nl,
         write('              4 - ����� �� ������ ��������'),nl,
         write('              5 - ����� �� ������'),nl,
         write('              6 - ����������� �����'),nl,         
         write('              7 - ����� � ������� ����'),nl,
         write('������� ��� �����: '),read_line(0,Y),run(Y).
         
run($1$):-write('������� �������: '),read_line(0,F),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),
        get0_noecho(X),do($4$).

run($2$):-write('������� ���: '),read_line(0,I),
        write('������� ��������: '),read_line(0,O),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),
        get0_noecho(X),do($4$).   

run($3$):-write('������� ���� ��������: '),read_line(0,D),
        write('������� ����� ��������: '),read_line(0,M),
        write('������� ��� ��������: '),read_line(0,G),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),
        get0_noecho(X),do($4$).   

run($4$):-write('������� ����� ��������: '),read_line(0,N),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),
        get0_noecho(X),do($4$).

run($5$):-write('������� ����� ����������: '),read_line(0,H),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),
        get0_noecho(X),do($4$).   

run($6$):-write('������� �������:'),read_line(0,F),
        write('������� �������� �����:'),read_line(0,J),
        zap(v(F,I,O),y(D,M,G),t(N),a(H)),ros(J,H),
        write(F),write(' '),write(I),write(' '),write(O),write('  '),
        write(D),write('.'),write(M),write('.'),write(G),write('  '),
        write('���.'),write(N),write('  '),write('��.'),
        write(H),write(' '),nl,fail.
run($6$):-get0_noecho(X),do($4$).           

run($7$):-menu.              

do($5$):- write('������� ������ �������: '),read_line(0,F),
          zap(v(F,I,O),y(D,M,G),t(N),a(H)),
          write('������� ��������� ������: '),
	  write(F),write(' '),write(I),write(' '),write(O),write('  '),
	  write(D),write('.'),write(M),write('.'),write(G),write('  '),
	  write('���.'),write(N),write('  '),write('��.'),
          write(H),write(' '),nl,
          write('������� ����� �������: '),read_line(0,F1),
          retract(zap(v(F,_,_),y(_,_,_),t(_),a(_))),
          assert(zap(v(F1,I,O),y(D,M,G),t(N),a(H))),menu.
do($5$):-write('����� ������� ���'),get0_noecho(X),menu.       

do($6$):- write('������� ������ �����: '),read_line(0,N),
          zap(v(F,I,O),y(D,M,G),t(N),a(H)),
          write('������� ��������� ������: '),
	  write(F),write(' '),write(I),write(' '),write(O),write('  '),
	  write(D),write('.'),write(M),write('.'),write(G),write('  '),
	  write('���.'),write(N),write('  '),write('��.'),
          write(H),write(' '),nl,
          write('������� ����� �����: '),read_line(0,N1),
          retract(zap(v(_,_,_),y(_,_,_),t(N),a(_))),
          assert(zap(v(F,I,O),y(D,M,G),t(N1),a(H))),menu.
do($6$):-write('������ ������ ���'),get0_noecho(X),menu.

do($7$):- write('������� ������ �����: '),read_line(0,H),
          zap(v(F,I,O),y(D,M,G),t(N),a(H)),
          write('������� ��������� ������: '),
	  write(F),write(' '),write(I),write(' '),write(O),write('  '),
	  write(D),write('.'),write(M),write('.'),write(G),write('  '),
	  write('���.'),write(N),write('  '),write('��.'),
          write(H),write(' '),nl,
          write('������� ����� �����: '),read_line(0,H1),
          retract(zap(v(_,_,_),y(_,_,_),t(_),a(H))),
          assert(zap(v(F,I,O),y(D,M,G),t(N),a(H1))),menu.
do($7$):-write('������ ������ ���'),get0_noecho(X),menu.     

do($8$):- save('fonspr.db'),
          retractall(_),
          write('                ���� ��������� � ���� FONSPR.DB'), nl,
          write('                ������� �� ������ � ����������'). 
     
ros($$,H):-!.
ros(W,H):-string_length(W,L),substring(W,0,1,G),
          string_search(G,H,I),L1 is L-1,substring(W,1,L1,Q),!,ros(Q,H).           

retractall(Fact):- retract(Fact),
                   fail.
retractall(_).

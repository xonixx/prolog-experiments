
% �� ������ �������� ��� �����: �������, ������ � �������. �� ������� �������,������ � �������. � ������� ��� �� ������� �� ������.
% �� ����� ������� �� ������. �������, ������� �� ������ ��������, ������ ������. ������� ������� �������, ������ � ��������!!


friends(Sl, Tok, Sv) :-
 All =  [borisov, ivanov, semenov],
 member(Sl, All),
 member(Tok, All),
 member(Sv, All),
 \+ (Sl = Tok), \+ (Tok = Sv), \+ (Sl = Sv).
 
solve(Sl, Tok, Sv) :-
 friends(Sl, Tok, Sv),
 \+ has_br_sist(Sl),
 % ages(sl/Sl, tok/Tok, sv/Sv).
 older()
 
% younger(sl/Sl, tok/Tok),
% younger(sl/Sl, sv/Sv),
% younger(tok/Tok, _/semenov).
 

has_br_sist(borisov).



%younger(A, B) :-
% younger(A, C),
% younger(C, B).
 

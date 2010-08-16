# coding: cp1251

A=["Калининград", "Вологда", "Алматы", "Дмитров",
   "Архангельск", "Тобольск", "Краков"]
_A=["Калининград","Кондопога","Александров","Всеволожск"]
_A=["Калининград", "Вологда", "Далматово", "Дмитров", "Архангельск", "Владивосток", "Краков"]

#r=[e for e in p(A) if o(e)]

#def o(e):return all(e[i] for i in len(e)-1)
#r=filter(lambda e:all(ord(e[i][-1])==32+ord(e[i+1][0])for i in range(len(e)-1)),p(A))

from itertools import permutations as p
from __builtin__ import len as l, ord as o, filter as f, range as r, all as a, any as n
#r=filter(lambda e:all(ord(e[i][-1])==32+ord(e[i+1][0])or ord(e[i][-2])==32+ord(e[i+1][0])and(1-any(ord(e[i][-1])==32+ord(w[0])for w in A))for i in range(len(e)-1)),p(A))
z=f(lambda e:a(o(e[i][-1])==32+o(e[i+1][0])or o(e[i][-2])==32+o(e[i+1][0])and(1-n(o(e[i][-1])==32+o(w[0])for w in A))for i in r(l(e)-1)),p(A))
#z=f(lambda e:a((o(e[i][-1])==32+o(e[i+1][0]))|(o(e[i][-2])==32+o(e[i+1][0]))&(1-n(o(e[i][-1])==32+o(w[0])for w in A))for i in r(l(e)-1)),p(A))
	



#print list(p(A))
#print z
print ', '.join(z[0])
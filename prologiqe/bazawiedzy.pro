malfunctions(X) if needs(X, Y) and malfunctions(Y).
malfunctions(X) if symptom(_, X) and not indirect(X).

indirect(X) if needs(X, Y) and malfunctions(Y).

needs(aparat,matryca) if true.
needs(aparat,kartaPamieci) if true.
needs(aparat,lampa) if true.
needs(matryca,ostrosc) if true.
needs(matryca,film) if true.
needs(matryca,paski) if true.
needs(kartaPamieci, komunikat) if true.
needs(kartaPamieci, iloscZdjec) if true.
needs(lampa,ciemneZdjecia) if true.
needs(ciemneZdjecia,trybLampy) if true.
needs(lampa,przetwornica) if true.
needs(przetwornica,tranzystory) if true.
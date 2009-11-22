
double([H,H | T]) -->
	[H], !,
	double(T).

double([]) --> [].

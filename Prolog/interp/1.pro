:- style_check(-atom).

a :-
	phrase(code(Code), "function fib(n){if (n<2){return(n)}else{return(fib(n-1)+fib(n-2))}};write(fib(10))").

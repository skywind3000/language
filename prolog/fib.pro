
fib(0, 1).
fib(1, 1).
fib(2, 2).

fib(N, V) :-
	N > 2, N1 is N - 1, N2 is N - 2, 
	fib(N1, V1), fib(N2, V2), V is V1 + V2.


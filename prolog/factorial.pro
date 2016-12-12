

factorial(0, 1).
factorial(1, 1).
factorial(2, 2).
factorial(3, 6).

factorial(N, V) :- 
	N > 3, N1 is N - 1,
	factorial(N1, V1), V is V1 * N.

factorial(N, V) :-
	V > 6,
	factorial(N1, V1), V is V1 * N, N1 is N - 1.



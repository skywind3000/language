:- use_module(library(clpfd)).

factorial(0,1).
factorial(N,F) :-
    N in 1..2147483647, % between 1 and max int
    N1 #= N - 1,
    factorial(N1,F1),
	N #>= 1,
    F #= N * F1.



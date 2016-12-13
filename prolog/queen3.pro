:-use_module(library(clpfd)).
:-include('plib.pro').

solve(P) :-
	perm([1,2,3,4,5,6,7,8],P),
	combine([1,2,3,4,5,6,7,8],P,S,D),
	different(S),
	different(D).

combine([X1|X], [Y1|Y], [S1|S], [D1|D]) :-
	S1 is X1 + Y1,
	D1 is X1 - Y1,
	combine(X, Y, S, D).

combine([], [], [], []).




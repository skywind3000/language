

less([], X, []).
less([A], X, [A]) :- A < X.
less([A], X, [A]) :- A = X.
less([A], X, []) :- A > X.

less([HA|LA], X, B) :-
	\+(LA=[]),
	less([HA], X, B1),
	less(LA, X, B2),
	append(B1, B2, B).

great([], X, []).
great([A], X, [A]) :- A > X.
great([A], X, []) :- A = X.
great([A], X, []) :- A < X.

great([HA|LA], X, B) :-
	\+(LA=[]),
	great([HA], X, B1),
	great(LA, X, B2),
	append(B1, B2, B).


qsort([], []).
qsort([X], [X]).

qsort([HA|LA], B) :-
	\+(LA=[]),
	less(LA, HA, A1),
	great(LA, HA, A2),
	qsort(A1, B1),
	qsort(A2, B2),
	append(B1, [HA], B3),
	append(B3, B2, B).




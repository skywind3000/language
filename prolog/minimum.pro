


minimum([], 0).
minimum([A], A).

min(A, B, C) :- A < B, C is A.
min(A, B, C) :- A = B, C is A.
min(A, B, C) :- A > B, C is B.

minimum([HA|LA], B) :-
	\+(LA=[]),
	minimum(LA, B1),
	min(HA, B1, B).




reverse([], []).
reverse([A], [A]).

reverse([HA|LA], B) :-
	\+(LA=[]),
	reverse(LA, B1),
	\+(B1=[]),
	append(B1, [HA], B).

reverse(A, [HB|LB]) :-
	\+(LB=[]),
	reverse(A1, LB),
	\+(A1=[]),
	append(A1, [HB], A).



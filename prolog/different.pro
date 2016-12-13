
exclude([], _).
exclude([A], B) :- \+(A=B).
exclude([HA|LA], B) :- \+(LA=[]), \+(HA=B), exclude(LA, B).

different([]).
different([_]).
different([HA|LA]) :- \+(LA=[]), exclude(LA, HA), different(LA).

my_all_different(X) :- different(X).



% prolog common lib
% author: skywind3000@163.com

car([A|_], A).
cdr([_|B], B).
cons(X, R, [X|R]).

sizeof([], 0).
sizeof([_|Y], S) :- sizeof(Y, S1), S is S1 + 1. 

concatinate([], A, A).
concatinate([H|L1], B, [H|L2]) :- concatinate(L1, B, L2).

include([X|_], X).
include([Y|R], X) :- \+(Y=X), include(R, X).

exclude([], _).
exclude([HA|LA], B) :- \+(HA=B), exclude(LA,B).

different([]).
different([HA|LA]) :- exclude(LA,HA), different(LA).

takeout(X, [X|R], R).
takeout(X, [F|R], [F|S]) :- takeout(X,R,S).


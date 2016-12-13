% Prolog Common Lib - skywind3000@gmail.com
%
% reference: https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_11.html
%

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

imp_reverse([], X, X).
imp_reverse([X|Y], Z, W) :- imp_reverse(Y, [X|Z], W).

reverse(X, Y) :- imp_reverse(X, [], Y).

perm([], []).
perm([X|Y], Z) :- perm(Y, W), takeout(X, Z, W).



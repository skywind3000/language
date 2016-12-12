male(di).
male(jianbo).
female(xin).
female(yuan).
female(yuqing).
father(jianbo, di).
father(di, yuqing).
mother(xin, di).
mother(yuan, yuqing).

grandfather(X, Y) :- father(X, Z), father(Z, Y).
grandmother(X, Y) :- mother(X, Z), mother(Z, Y).
daughter(X, Y) :- father(X, Y),female(Y).
son(X, Y) :- father(X, Y),male(Y).


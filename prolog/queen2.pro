:-use_module(library(clpfd)).

valid_queen((_, Col)) :- member(Col, [1,2,3,4,5,6,7,8]).

valid_board([]).
valid_board([Head|Tail]) :- valid_queen(Head), valid_board(Tail).


cols([], []).
cols([(_, Col)|QueenTail], [Col|ColTail]) :-
	cols(QueenTail, ColTail).

diags1([], []).
diags1([(Row, Col)|QueenTail], [DH|DL]) :-
	DH is Col - Row,
	diags1(QueenTail, DL).

diags2([], []).
diags2([(Row, Col)|QueenTail], [DH|DL]) :-
	DH is Col + Row,
	diags2(QueenTail, DL).

eight_queens(Board) :-
	Board = [(1,_),(2,_),(3,_),(4,_),(5,_),(6,_),(7,_),(8,_)],
	valid_board(Board),

	cols(Board, Cols),
	diags1(Board, Diags1),
	diags2(Board, Diags2),

	all_different(Cols),
	all_different(Diags1),
	all_different(Diags2).

solve :-
	Board = [(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)],
	eight_queens(Board),
	List = [A, B, C, D, E, F, G, H],
	write(List),
	nl.


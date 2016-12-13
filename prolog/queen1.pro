:-use_module(library(clpfd)).

valid_queen((Row, Col)) :-
	Range = [1,2,3,4,5,6,7,8],
	member(Row, Range), member(Col, Range).

valid_queen2((R, C)) :- [R, C] ins 1..8.

valid_board([]).
valid_board([Head|Tail]) :- valid_queen(Head), valid_board(Tail).

rows([], []).
rows([(Row, _)|QueenTail], [Row|RowTail]) :-
	rows(QueenTail, RowTail).

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
	length(Board, 8),
	valid_board(Board),

	rows(Board, Rows),
	cols(Board, Cols),
	diags1(Board, Diags1),
	diags2(Board, Diags2),

	all_different(Rows),
	all_different(Cols),
	all_different(Diags1),
	all_different(Diags2).



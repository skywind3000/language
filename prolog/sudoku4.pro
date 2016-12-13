:-use_module(library(clpfd)).
% valid_domain(List, Min, Max) :-
%   flatten(List, Temp), Temp ins Min..Max.

dd(A) :- all_different(A).

valid([]).
valid([Head|Tail]) :-
	all_different(Head),
	valid(Tail).

valid_domain(A) :- 
	A ins 1..4.

sudoku(Puzzle, Solution) :-
	Solution = Puzzle,
	Puzzle = [	S11, S12, S13, S14,
				S21, S22, S23, S24,
				S31, S32, S33, S34,
				S41, S42, S43, S44],
	
	% valid_domain(Solution, 1, 4),
	valid_domain(Solution),

	Row1 = [S11, S12, S13, S14],
	Row2 = [S21, S22, S23, S24],
	Row3 = [S31, S32, S33, S34],
	Row4 = [S41, S42, S43, S44],

	Col1 = [S11, S21, S31, S41],
	Col2 = [S12, S22, S32, S42],
	Col3 = [S13, S23, S33, S43],
	Col4 = [S14, S24, S34, S44],

	Sq1 = [S11, S12, S21, S22],
	Sq2 = [S13, S14, S23, S24],
	Sq3 = [S31, S32, S41, S42],
	Sq4 = [S33, S34, S43, S44],

	valid([Row1, Row2, Row3, Row4,
			Col1, Col2, Col3, Col4,
			Sq1, Sq2, Sq3, Sq4]).

prow(X) :-
	[A, B, C, D] = X,
	write(A), write(' '),
	write(B), write(' '),
	write(C), write(' '),
	write(D), nl.

solve(Puzzle) :-
	sudoku(Puzzle, S),
	S = [	S11, S12, S13, S14,
			S21, S22, S23, S24,
			S31, S32, S33, S34,
			S41, S42, S43, S44],
	prow([S11, S12, S13, S14]),
	prow([S21, S22, S23, S24]),
	prow([S31, S32, S33, S34]),
	prow([S41, S42, S43, S44]),
	nl.
	



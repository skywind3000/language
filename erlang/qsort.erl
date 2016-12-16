-module(qsort).
-export([main/1, qsort/1]).


qsort([]) ->
	[];
qsort([H|L]) ->
	A = [X || X <- L, X < H],
	B = [X || X <- L, X > H],
	C = [X || X <- L, X == H],
	lists:append([qsort(A), [H], C, qsort(B)]).


main(_) ->
	io:format("~p~n", [qsort([3,1,4,1,5,9,2,6,5,3,5])]),
	ok.




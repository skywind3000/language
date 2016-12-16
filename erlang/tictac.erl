-module(tictac).
-export([main/1]).

check([X, X, X]) -> X;
check([_, _, _]) -> nil.

count([], _) -> 0;
count([X|L], X) -> 1 + count(L, X);
count([_|L], X) -> count(L, X).

tictac([N1, N2, N3, N4, N5, N6, N7, N8, N9]) ->
	R1 = [N1, N2, N3],
	R2 = [N4, N5, N6],
	R3 = [N7, N8, N9],
	C1 = [N1, N3, N7],
	C2 = [N2, N5, N8],
	C3 = [N3, N6, N9],
	S1 = [N1, N5, N9],
	S2 = [N3, N5, N7],
	NN = [R1, R2, R3, C1, C2, C3, S1, S2],
	TT = lists:append([R1, R2, R3]),
	Nx = count(TT, x),
	No = count(TT, o),
	MM = [ check(X) || X <- NN ],
	Wx = count(MM, x),
	Wo = count(MM, o),
	% io:format("Nx:~p, Ny:~p~n", [Nx, No]),
	% io:format("Wx:~p, Wy:~p~n", [Wx, Wo]),
	if 
		Wx > 0 ->
			x;
		Wo > 0 ->
			o;
		Nx + No == 9 ->
			cat;
		true ->
			no_winner
	end.


main(_) ->
	B1 = [	o, n, n,
			x, n, x,
			n, o, o],
	B2 = [	o, n, n,
			x, o, x,
			n, o, o],
	B3 = [	o, n, n,
			x, x, x,
			n, o, o],
	B4 = [	o, x, o,
			o, x, o,
			x, o, x],
	R1 = tictac(B1),
	io:format("Result of Game is ~p~n", [R1]),
	R2 = tictac(B2),
	io:format("Result of Game is ~p~n", [R2]),
	R3 = tictac(B3),
	io:format("Result of Game is ~p~n", [R3]),
	R4 = tictac(B4),
	io:format("Result of Game is ~p~n", [R4]),
	ok.



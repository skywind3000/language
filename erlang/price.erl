-module(price).
-export([main/1]).


main(_) ->
	Cart = [{pencil, 4, 0.2}, {pen, 1, 1.20}, {paper, 2, 0.2}],
	io:format("~p~n", [Cart]),
	Price = [{Name, N * P} || {Name, N, P} <- Cart],
	io:format("~p~n", [Price]),
	ok.



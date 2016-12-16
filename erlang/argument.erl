-module(argument).
-export([main/1]).


main(X) ->
	io:format("~p~n", [X]),
	ok.


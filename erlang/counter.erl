-module(counter).

-export([main/1]).


count(N, N) ->
	true;
count(I, N) ->
	io:format("~w~n", [I]),
	count(I + 1, N).


main(_) ->
	count(0, 10),
	halt(0).



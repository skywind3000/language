-module(nano).

-export([len/1, concat/1, concat/2]).
-export([main/1]).

len([]) -> 0;
len([_|X]) -> 1 + len(X).

concat(A, B) -> A ++ B.

concat([E]) -> E;
concat([H|T]) -> H ++ concat(T);
concat([]) -> [].


main(_) ->
	io:format("~w~n", [len([1,2,3,4])]),
	io:format("~w~n", [len("HELLO")]),
	io:format("~w~n", [len(concat([1,2,3], [4,5]))]),
	io:format("~ts~n", [concat("Hello", " World !!")]),
	halt(0).



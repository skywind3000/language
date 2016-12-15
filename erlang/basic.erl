

-module(basic).
-export([mirror/1]).

mirror(Anything) -> Anything.

main(_) ->
	io:format("Fuck\n"),
	halt(0).



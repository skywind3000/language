-module(message).
-export([main/1]).


detect(success) ->
	io:format("success~n"),
	success;
detect({error, Message}) ->
	io:format("error: ~ts~n", [Message]).


main(_) ->
	detect(success),
	detect({error, "don't touch me here"}).



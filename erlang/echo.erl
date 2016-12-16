-module(echo).
-author('skywind3000').
-export([listen/1, main/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(ListenSocket).

accept(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> loop(Socket) end),
	accept(ListenSocket).


loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			gen_tcp:send(Socket, Data),
			loop(Socket);
		{error, closed} ->
			ok
	end.

main(_) ->
	io:format("Listen on port 2000\n"),
	listen(2000),
	halt(0).



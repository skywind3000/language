-module(log_server).
-export([main/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	register(recorder, spawn(fun() -> record() end)),
	io:format("listen on port ~p~n", [Port]),
	accept(ListenSocket).


accept(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> loop(Socket) end),
	accept(ListenSocket).

loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			recorder ! {log, Data},
			loop(Socket);
		{error, closed} ->
			ok
	end.

record() ->
	{ok, F} = file:open("log_server.txt", write),
	logging(F).

logging(F) ->
	receive
		{log, What} ->
			io:format(F, "~p~n", [What]),
			io:format("record: ~p~n", [What]),
			logging(F)
	end.


main(_) ->
	listen(2000),
	ok.



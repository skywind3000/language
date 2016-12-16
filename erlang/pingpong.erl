-module(pingpong).
-export([main/1]).


ping(Pong_Pid, 0) ->
	Pong_Pid ! finished,
	io:format("ping finished~n");
ping(Pong_Pid, N) ->
	Pong_Pid ! {self(), ping},
	receive
		pong ->
			io:format("ping received pong~n")
	end,
	ping(Pong_Pid, N - 1).

pong() ->
	receive
		finished ->
			io:format("pong finished~n");
		{Ping_Pid, ping} ->
			io:format("pong received ping~n"),
			Ping_Pid ! pong,
			pong()
	end.

wait()->wait().

main(_) ->
	Pong_Pid = spawn(fun() -> pong() end),
	spawn(fun() -> ping(Pong_Pid, 3) end),
	wait().



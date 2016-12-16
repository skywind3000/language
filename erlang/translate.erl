-module(translate).
-export([loop/0, main/1]).

loop() ->
	receive
		"casa" ->
			io:format("house~n"),
			loop();

		"blanca" ->
			io:format("white~n"),
			loop();

		"exit" ->
			io:format("exit here~n"),
			ok;

		_ ->
			io:format("I don't understand.~n"),
			loop()
	end.

wait() ->
	wait().

main(_) ->
	Pid = spawn(fun() -> loop() end),
	Pid ! "casa",
	Pid ! "blanca",
	Pid ! "dfdfdf",
	io:format("Pid: ~p~n", [Pid]),
	Pid ! "exit",
	Pid ! "casa",
	Pid ! "blanca",
	Pid ! "dfdfdf",
	io:format("Pid: ~p~n", [Pid]),
	wait(),
	ok.


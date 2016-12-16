-module(coroner).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		{monitor, Process} ->
			link(Process),
			io:format("Monitoring process ~p~n", [Process]),
			loop();

		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p. ", [From, Reason]),
			io:format("Start another one.~n"),
			loop()
	end.



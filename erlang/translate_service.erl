-module(translate_service).
-export([loop/0, translate/2, main/1]).



loop() ->
	receive
		{From, "casa"} ->
			From ! "house",
			loop();

		{From, "blanca"} ->
			From ! "white",
			loop();

		{From, "exit"} ->
			From ! "exit here",
			ok;

		{From, _} -> 
			From ! "I don't understand.",
			loop()
	end.


translate(To, Word) ->
	To ! { self(), Word},
	receive
		Translation -> 
			Translation
	end.


wait() -> 
	wait().

main(_) ->
	Translator = spawn(fun () -> loop() end),
	Translator ! "SomeThing",
	P1 = translate(Translator, "blanca"),
	io:format("~p~n", [P1]),
	P2 = translate(Translator, "casa"),
	io:format("~p~n", [P2]),
	P3 = translate(Translator, "tasa"),
	io:format("~p~n", [P3]),
	P4 = translate(Translator, "exit"),
	io:format("~p~n", [P4]),
	P5 = translate(Translator, "exit"),
	io:format("~p~n", [P5]),
	ok.



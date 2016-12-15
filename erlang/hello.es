-import(basic).

main(_) ->
	io:format("Hello, Erlang World !!\n"),
	X = basic:mirror(smiling_mug),
	io:format(X),
	io:format("\n"),
	halt(1).


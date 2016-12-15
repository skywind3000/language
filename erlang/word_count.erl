-module(word_count).
-export([space/1, alnum/1, count_word/1, main/1]).

space(9) -> true;
space(32) -> true;
space(13) -> true;
space(10) -> true;
space(' ') -> true;
space('\r') -> true;
space('\n') -> true;
space('\t') -> true;
space(_) -> false.

alnum(X) -> not(space(X)).

count_in_space([]) -> 0;
count_in_space([H|L]) ->
	X = alnum(H),
	if 
		X == true -> 
			count_in_word([H|L]) + 1;
		true -> 
			count_in_space(L)
	end.

count_in_word([]) -> 0;
count_in_word([H|L]) ->
	X = alnum(H),
	if 
		X == true -> count_in_word(L);
		true -> count_in_space([H|L])
	end.

count_word([]) -> 0;
count_word([H|L]) -> 
	X = alnum(H),
	if 
		X == true ->
			count_in_word([H|L]) + 1;
		true -> 
			count_in_space(L)
	end.


main(_) ->
	io:format("word counting\n"),
	io:format(alnum(' ')),
	io:format("\n"),
	io:format(alnum('t')),
	io:format("\n"),
	io:format("count: ~w~n", [count_word("x sdf sf s")]),
	halt(1).



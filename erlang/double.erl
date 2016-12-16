% just another test
-module(just_another).
-export([main/1]).


double_all([]) -> [];
double_all([H|L]) -> [H + H|double_all(L)].

main(_) ->
	io:format("~p~n", [double_all([1,2,3])]),
	ok.


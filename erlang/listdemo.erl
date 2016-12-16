-module(listdemo).
-export([main/1, getvalue/2, getvalue2/2]).


getvalue([], _) -> [];
getvalue([H|L], K) ->
	{Key, Value} = H,
	if 
		Key == K ->
			[Value|getvalue(L, K)];
		true ->
			getvalue(L, K)
	end.

getvalue2([], _) -> [];
getvalue2([{K, Value}|L], K) ->
	[Value|getvalue2(L, K)];
getvalue2([{_, _}|L], K) ->
	getvalue2(L, K).


main(_) ->
	X = [{erlang, "a functional language"}, {ruby, "an OO language"}],
	Y = getvalue2(X, erlang),
	Z = getvalue2(X, ruby),
	io:format("~p~n", [Y]),
	io:format("~p~n", [Z]),
	ok.



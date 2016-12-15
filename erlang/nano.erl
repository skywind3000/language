-module(nano).

-export([len/1,  concat/2]).
-export([main/1]).

len([]) -> 0;
len([_|X]) -> 1 + len(X).

concat([], E) -> E;
concat([H|T], E) -> [H|concat(T, E)].

reverse([]) -> [];
reverse([E]) -> [E];
reverse([A, B]) -> [B, A];
reverse([H|L]) when length(L) > 1 -> concat(reverse(L), [H]).

space(9) -> true;
space(32) -> true;
space(13) -> true;
space(10) -> true;
space(' ') -> true;
space('\r') -> true;
space('\n') -> true;
space('\t') -> true;
space(_) -> false.

alnum(N) -> not(space(N)).

lstrip([]) -> [];
lstrip([H|T]) ->
	X = space(H),
	if 
		X == true -> lstrip(T);
		true -> [H|T]
	end.

rstrip(X) -> reverse(lstrip(reverse(X))).
strip(X) -> lstrip(rstrip(X)).


main(_) ->
	io:format("~w~n", [len([1,2,3,4])]),
	io:format("~w~n", [len("HELLO")]),
	io:format("~w~n", [len(concat([1,2,3], [4,5]))]),
	io:format("~ts~n", [concat("Hello", " World !!")]),
	io:format("~ts~n", [reverse("Hello World !!")]),
	io:format("lstrip: <~ts>~n", [lstrip("   asdfasdf df d  ")]),
	io:format("lstrip: <~ts>~n", [lstrip("x   asdfasdf df d  ")]),
	io:format("rstrip: <~ts>~n", [rstrip("   asdfasdf df d  ")]),
	io:format("strip: <~ts>~n", [strip("   asdfasdf df d  ")]),
	halt(0).



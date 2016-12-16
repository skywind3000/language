-module(std).

-export([len/1, concat/1, concat/2, space/1, alnum/1, reverse/1]).
-export([lstrip/1, rstrip/1, strip/1]).
-export([main/1]).

% list length
len([]) -> 0;
len([_|X]) -> 1 + len(X).

% list concatenate
concat([], E) -> E;
concat([H|T], E) -> [H|concat(T, E)].

% list concatenate nested version
concat([]) -> [];
concat([H|L]) -> concat(H, concat(L)).

% reverse list
reverse(E) -> reverse(E, []).

reverse([], E) -> E;
reverse([H|L], E) -> reverse(L, [H|E]).


% check space character
space(9) -> true;
space(32) -> true;
space(13) -> true;
space(10) -> true;
space(' ') -> true;
space('\r') -> true;
space('\n') -> true;
space('\t') -> true;
space(_) -> false.

% check non-space
alnum(N) -> not(space(N)).

% strip spaces
strip(X) -> lstrip(rstrip(X)).
lstrip([]) -> [];
lstrip([H|T]) ->
	X = space(H),
	if 
		X == true -> lstrip(T);
		true -> [H|T]
	end.

rstrip(X) -> reverse(lstrip(reverse(X))).

% generate repeated list
repeat_list([X], 0) -> [];
repeat_list([X], N) -> [X|repeat_list([X], N - 1)].

% loop macro
loop(N, N, Fn) -> ok;
loop(I, N, Fn) when I > N -> ok;
loop(I, N, Fn) -> 
	case Fn(I) of 
		break ->
			false;
		false ->
			false;
		_ -> loop(I + 1, N, Fn)
	end.


% find min
find_min([]) -> nil;
find_min([X]) -> X;
find_min([H|L]) ->
	Y = find_min(L),
	if
		H < Y -> H;
		true -> Y
	end.

% find max
find_max([]) -> nil;
find_max([X]) -> X;
find_max([H|L]) ->
	Y = find_max(L),
	if 
		H > Y -> H;
		true -> Y
	end.

% qsort
qsort([]) ->
	[];
qsort([H|L]) ->
	X = [N || N <- L, N < H],
	Y = [N || N <- L, N > H],
	Z = [N || N <- L, N == H],
	concat([qsort(X), [H], Z, qsort(Y)]).


% testing case
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
	io:format("repeat: <~ts>~n", [repeat_list("X", 1)]),
	loop(0, 10, fun(X) -> io:format("loop: ~w~n", [X]), false2 end),
	io:format("min: ~p~n", [find_min([3,1,4,1,5,9,2,6])]),
	io:format("max: ~p~n", [find_max([3,1,4,1,5,9,2,6])]),
	io:format("sort: ~p~n", [qsort([3,1,4,1,5,9,2,6])]),
	halt(0).



-module(tcp_forward).
-export([main/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {exit_on_close, false}]).


listen(DestIp, DestPort, LocalPort) ->
	{ok, ListenSocket} = gen_tcp:listen(LocalPort, ?TCP_OPTIONS),
	T = [LocalPort, DestIp, DestPort],
	io:format("Forwarding localhost:~p to ~ts:~p~n", T),
	accept(DestIp, DestPort, ListenSocket).

accept(DestIp, DestPort, ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	P = spawn(fun() -> agent(DestIp, DestPort, Socket) end),
	ok = gen_tcp:controlling_process(Socket, P),
	% tell agent/3, I have passed over control privilege to you.
	P ! continue,
	accept(DestIp, DestPort, ListenSocket).


agent(DestIp, DestPort, SrcSocket) ->
	{ok, DstSocket} = gen_tcp:connect(DestIp, DestPort, ?TCP_OPTIONS),
	io:format("connection to ~ts:~p established~n", [DestIp, DestPort]),
	% wait for accept/3 give me the control privilege
	receive
		continue -> ok
	end,
	P1 = spawn(fun() -> proxy_recv(SrcSocket, DstSocket) end),
	P2 = spawn(fun() -> proxy_send(SrcSocket, DstSocket) end),
	io:format("reader/writer pid is ~p/~p ~n", [P1,P2]),
	ok = gen_tcp:controlling_process(DstSocket, P2),
	ok = gen_tcp:controlling_process(SrcSocket, P1),
	ok.

proxy_recv(SrcSocket, DstSocket) ->
	{ok, Data} = gen_tcp:recv(SrcSocket, 0),
	ok = gen_tcp:send(DstSocket, Data),
	proxy_recv(SrcSocket, DstSocket).

proxy_send(SrcSocket, DstSocket) ->
	{ok, Data} = gen_tcp:recv(DstSocket, 0),
	io:format("received return: ~p bytes~n", [size(Data)]),
	ok = gen_tcp:send(SrcSocket, Data),
	proxy_send(SrcSocket, DstSocket).



main(_) ->
	listen("192.168.1.2", 80, 2000).



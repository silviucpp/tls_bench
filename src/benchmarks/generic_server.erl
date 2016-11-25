-module(generic_server).

-include("tls_bench.hrl").

-export([start/1]).

start(Module) ->
    {ok, ServersConfs} = tlsb_utils:env(servers),
    Config = tlsb_config:get_config(Module, ServersConfs),
    server(Module, Config).

server(Module, Config) ->
    Port = tlsb_utils:lookup(listen_port, Config),
    ListenOpt = tlsb_utils:lookup(listen_opt, Config),
    TlsOpt = tlsb_utils:lookup(tls_opt, Config),
    TcpOpt = tlsb_utils:lookup(tcp_opt, Config),
    Acceptors = tlsb_utils:lookup(acceptors, Config),

    {ok, LSocket} = essl:listen(Module, Port, ListenOpt, TlsOpt),

    ?INFO_MSG("~p start listening on port ~p with ~p acceptors", [Module, Port, Acceptors]),
    ?INFO_MSG("~p listen_opt: ~p", [Module, ListenOpt]),
    ?INFO_MSG("~p tls_opt: ~p", [Module, TlsOpt]),
    ?INFO_MSG("~p tcp_opt: ~p", [Module, TcpOpt]),

    lists:foreach(fun(_) -> spawn(fun() -> accept(LSocket, TcpOpt) end) end, lists:seq(1, Acceptors)).

accept(LSocket, TcpOpt) ->
    {ok, Socket} = essl:accept(LSocket),

    LoopFun = fun() ->
        %?INFO_MSG("Connection accepted: ~p", [self()]),
        {ok, NewSock} = essl:handshake(Socket),
        ok = essl:setopts(NewSock, TcpOpt),
        loop(NewSock)
    end,

    ok = essl:controlling_process(Socket, spawn(LoopFun)),
    accept(LSocket, TcpOpt).

loop(Socket) ->
    case essl:recv(Socket) of
        {essl, Socket, Data} ->
            %?INFO_MSG("Got SSL packet: ~p", [Data]),
            ok = essl:send(Socket, Data),
            essl:setopts(Socket, [{active, once}]),
            loop(Socket);
        {essl_closed, Sock} ->
            %?INFO_MSG("Closing SSL socket: ~p", [Sock]),
            {ok, Sock};
        Error ->
            ?ERROR_MSG("Error on server socket: ~p", [Error])
    end.
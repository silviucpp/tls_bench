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
    ?INFO_MSG("~p listen_opt: ~200p", [Module, ListenOpt]),
    ?INFO_MSG("~p tls_opt: ~200p", [Module, TlsOpt]),
    ?INFO_MSG("~p tcp_opt: ~200p", [Module, TcpOpt]),
    ?INFO_MSG("~p tcp buffering: ~200p", [Module, essl:getopts(LSocket, [recbuf, sndbuf, buffer])]),

    lists:foreach(fun(_) -> spawn(fun() -> accept(LSocket, TcpOpt) end) end, lists:seq(1, Acceptors)).

accept(LSocket, TcpOpt) ->
    {ok, Socket} = essl:accept(LSocket),

    ClientProcess = spawn(fun() -> client_process(TcpOpt) end),

    case essl:controlling_process(Socket, ClientProcess) of
        ok ->
            ClientProcess ! {attach_socket, Socket};
        _ ->
            ?ERROR_MSG("failed to set socket ~p control process: ~p", [Socket, ClientProcess]),
            ClientProcess ! stop
    end,

    accept(LSocket, TcpOpt).

client_process(TcpOpt) ->
    receive
        {attach_socket, Socket} ->
            %?INFO_MSG("Connection accepted: ~p", [self()]),
            case essl:handshake(Socket) of
                ok ->
                    ok = essl:setopts(Socket, TcpOpt),
                    loop(Socket);
                Unexpected ->
                    ?ERROR_MSG("handshake failed:~p", [Unexpected]),
                    essl:close(Socket)
            end;
        stop ->
            ok
    after 30000 ->
        ?ERROR_MSG("No start_looping received in 30 seconds. ", [])
    end.

loop(Socket) ->
    case essl:recv(Socket) of
        {essl, Socket, Data} ->
            %?INFO_MSG("Got SSL packet: ~p", [Data]),
            case Data of
                <<>> ->
                    ok;
                _ ->
                    ok = essl:send(Socket, Data)
            end,
            essl:setopts(Socket, [{active, once}]),
            loop(Socket);
        {essl_closed, Sock} ->
            %?INFO_MSG("Closing SSL socket: ~p", [Sock]),
            {ok, Sock};
        Error ->
            ?ERROR_MSG("Error on server socket: ~p", [Error])
    end.
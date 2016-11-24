-module(ssl_client).

-include("tls_bench.hrl").

-export([benchmark/5]).

benchmark(ClientMod, Port, ConcurrentConnections, Requests, MessageLength) ->
    code_loader:start(),
    {ok, _} = application:ensure_all_started(tls_bench),

    {ok, ClientOpt} = tlsb_utils:env(client),
    Host = tlsb_utils:lookup(host, ClientOpt),
    ConnectTimeout = tlsb_utils:lookup(connect_timeout, ClientOpt),
    RecvTimeout = tlsb_utils:lookup(recv_timeout, ClientOpt),
    TcpOpt = tlsb_utils:lookup(tcp_opt, ClientOpt),
    TlsOpt = tlsb_config:get_tls_opt(ClientMod, ClientOpt),

    ReqPerConnection = round(Requests/ ConcurrentConnections),
    Message = Message = <<0:MessageLength/little-signed-integer-unit:8>>,
    SeqPerClient = lists:seq(1, ReqPerConnection),

    ?INFO_MSG("## start testing on ~p:~p", [Host, Port]),
    ?INFO_MSG("## requests per connection: ~p msg length: ~p total requests: ~p concurrency level: ~p", [ReqPerConnection, MessageLength, Requests, ConcurrentConnections]),

    ClientFun = fun() ->
        {ok, Socket} = essl:connect(ClientMod, Host, Port, TcpOpt, TlsOpt, ConnectTimeout),

        SendFun = fun(_) ->
            ok = essl:send(Socket, Message)
        end,

        ok = lists:foreach(SendFun, SeqPerClient),
        recv(Socket, ReqPerConnection*MessageLength, RecvTimeout)
    end,

    {Ts, _} = timer:tc(fun() -> multi_spawn:do_work(ClientFun, ConcurrentConnections) end),

    ?INFO_MSG(<<"## completed in :~p ms">>, [Ts/1000]).

recv(_Socket, 0, _Timeout) ->
    ok;
recv(Socket, Bytes, Timeout) ->
    case essl:recv(Socket, Timeout) of
        {essl, Socket, Data} ->
            recv(Socket, Bytes - byte_size(Data), Timeout);
        {essl_closed, Socket} ->
            ok;
        Error ->
            ?ERROR_MSG("Client received unexpected msg: ~p",[Error]),
            {error, Error}
    end.
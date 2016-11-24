-module(essl).
-author("silviu.caragea").

-include("tls_bench.hrl").

-record(state, {socket, mod, tls_opt}).

-export([
    connect/6,
    listen/4,
    accept/1,
    handshake/1,
    setopts/2,
    controlling_process/2,
    send/2,
    recv/1,
    recv/2
]).

connect(Mod, Host, Port, TcpOptions, TlsOptions, Timeout) ->
    Resp = case Mod of
        ?MOD_ETLS ->
            etls:connect(Host, Port, TcpOptions ++ TlsOptions, Timeout);
        ?MOD_FAST_TLS ->
            {ok, TcpSocket} = gen_tcp:connect(Host, Port, TcpOptions, Timeout),
            {ok, TlsSocket} = fast_tls:tcp_to_tls(TcpSocket, TlsOptions),
            ok = inet:setopts(TlsSocket, TlsOptions),
            {ok, TlsSocket};
        ?MOD_P1_TLS ->
            {ok, TcpSocket} = gen_tcp:connect(Host, Port, TcpOptions, Timeout),
            {ok, TlsSocket} = p1_tls:tcp_to_tls(TcpSocket, TlsOptions),
            ok = inet:setopts(TlsSocket, TlsOptions),
            {ok, TlsSocket};
        ?MOD_SSL ->
            ssl:connect(Host, Port, TcpOptions ++ TlsOptions, Timeout);
        ?MOD_TCP ->
            gen_tcp:connect(Host, Port, TcpOptions, Timeout)
    end,

    case Resp of
        {ok, Socket} ->
            {ok, #state {socket = Socket, mod = Mod}};
        _ ->
            Resp
    end.

listen(Mod, Port, ListenOptions, TlsOptions) ->
    ListenOpt = get_listen_options(Mod, TlsOptions, ListenOptions),

    Resp = case Mod of
        ?MOD_ETLS ->
            etls:listen(Port, ListenOpt);
        ?MOD_SSL ->
            ssl:listen(Port, ListenOpt);
        _ ->
            gen_tcp:listen(Port, ListenOpt)
    end,

    case Resp of
        {ok, Socket} ->
            {ok, #state {socket = Socket, mod = Mod, tls_opt = TlsOptions}};
        _ ->
            Resp
    end.

accept(#state{socket = LSocket, tls_opt = TlsOpt, mod = Mod}) ->
    Resp = case Mod of
        ?MOD_ETLS ->
            etls:accept(LSocket);
        ?MOD_SSL ->
            ssl:transport_accept(LSocket);
        _ ->
            gen_tcp:accept(LSocket)
    end,

    case Resp of
        {ok, Socket} ->
            {ok, #state {socket = Socket, mod = Mod, tls_opt = TlsOpt}};
        _ ->
            Resp
    end.

handshake(#state{socket = Socket, tls_opt = TlsOpt, mod = Mod} = State) ->
    Resp = case Mod of
        ?MOD_ETLS ->
            etls:handshake(Socket);
        ?MOD_FAST_TLS ->
            fast_tls:tcp_to_tls(Socket, TlsOpt);
        ?MOD_P1_TLS ->
            p1_tls:tcp_to_tls(Socket, TlsOpt);
        ?MOD_SSL ->
            ssl:ssl_accept(Socket);
        ?MOD_TCP ->
            ok
    end,

    case Resp of
        ok ->
            {ok, State#state{tls_opt = undefined}};
        {ok, TlsSocket} ->
            {ok, State#state{socket = TlsSocket, tls_opt = undefined}};
        _ ->
            Resp
    end.

setopts(#state{socket = Socket, mod = Mod}, Options) ->
    case Mod of
        ?MOD_ETLS ->
            etls:setopts(Socket, Options);
        ?MOD_FAST_TLS ->
            fast_tls:setopts(Socket, Options);
        ?MOD_P1_TLS ->
            p1_tls:setopts(Socket, Options);
        ?MOD_SSL ->
            ssl:setopts(Socket, Options);
        ?MOD_TCP ->
            inet:setopts(Socket, Options)
    end.

controlling_process(#state{socket = Socket, mod = Mod}, Pid) ->
    case Mod of
        ?MOD_ETLS ->
            etls:controlling_process(Socket, Pid);
        ?MOD_FAST_TLS ->
            case is_tuple(Socket) of
                true ->
                    fast_tls:controlling_process(Socket, Pid);
                _ ->
                    gen_tcp:controlling_process(Socket, Pid)
            end;
        ?MOD_P1_TLS ->
            case is_tuple(Socket) of
                true ->
                    p1_tls:controlling_process(Socket, Pid);
                _ ->
                    gen_tcp:controlling_process(Socket, Pid)
            end;
        ?MOD_SSL ->
            ssl:controlling_process(Socket, Pid);
        ?MOD_TCP ->
            gen_tcp:controlling_process(Socket, Pid)
    end.

send(#state{socket = Socket, mod = Mod}, Data) ->
    case Mod of
        ?MOD_ETLS ->
            etls:send(Socket, Data);
        ?MOD_FAST_TLS ->
            fast_tls:send(Socket, Data);
        ?MOD_P1_TLS ->
            p1_tls:send(Socket, Data);
        ?MOD_SSL ->
            ssl:send(Socket, Data);
        ?MOD_TCP ->
            gen_tcp:send(Socket, Data)
    end.

recv(State) ->
    recv(State, infinity).

recv(#state{socket = Socket} = State, Timeout) ->
    receive

        {etls, Socket, Data} ->
            {essl, State, Data};
        {ssl, Socket, Data} ->
            {essl, State, Data};
        {tcp, TcpSocket, TlsData} ->
            read_tcp(TcpSocket, State, TlsData, Timeout);

        {etls_closed, Socket} ->
            {essl_closed, State};
        {ssl_closed, Socket} ->
            {essl_closed, State};
        {tcp_closed, _Sock} ->
            {essl_closed, State};

        UnexpectedMsg ->
            UnexpectedMsg
    after Timeout ->
        {error, Timeout}
    end.

% private methods

read_tcp(TcpSocket, #state{mod = Mod, socket = TlsSocket} = State, TlsData, Timeout) ->
    {ok, Data} = case Mod of
         ?MOD_FAST_TLS ->
             fast_tls:recv_data(TlsSocket, TlsData);
         ?MOD_P1_TLS ->
             p1_tls:recv_data(TlsSocket, TlsData);
         ?MOD_TCP ->
             {ok, TlsData}
    end,

    case Data of
        <<>> ->
            %tls handshake process or tls close. read more data until we get something
            {ok, [{active, CurrentActive}]} = inet:getopts(TcpSocket, [active]),
            ok = inet:setopts(TcpSocket, [{active, false}]),

            case read_packet(Mod, TlsSocket, Timeout) of
                {ok, Bin} ->
                    ok = inet:setopts(TcpSocket, [{active, CurrentActive}]),
                    {essl, State, Bin};
                {error, closed} ->
                    {essl_closed, State};
                {errr, Unexpected} ->
                    Unexpected
            end;
        _ ->
            {essl, State, Data}
    end.

read_packet(Mod, Socket, Timeout) ->
    case Mod of
        ?MOD_FAST_TLS ->
            fast_tls:recv(Socket, 0, Timeout);
        ?MOD_P1_TLS ->
            p1_tls:recv(Socket, 0, Timeout)
 end.

get_listen_options(Mod, TlsOpt, ListenOpt) ->
    case Mod =:= ?MOD_ETLS orelse Mod =:= ?MOD_SSL of
        true ->
            ListenOpt ++ TlsOpt;
        _ ->
            ListenOpt
    end.
-module(essl).
-author("silviu.caragea").

-include("tls_bench.hrl").

-record(state, {socket, mod, tls_opt}).
-record(tlssock, {tcpsock :: inet:socket(), tlsport :: port()}).

-export([
    connect/6,
    listen/4,
    accept/1,
    handshake/1,
    setopts/2,
    getopts/2,
    controlling_process/2,
    close/1,
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
            {ok, TlsSocket} = fast_tls:tcp_to_tls(TcpSocket, [connect|TlsOptions]),
            {ok, TlsSocket};
        ?MOD_P1_TLS ->
            {ok, TcpSocket} = gen_tcp:connect(Host, Port, TcpOptions, Timeout),
            {ok, TlsSocket} = p1_tls:tcp_to_tls(TcpSocket, [connect|TlsOptions]),
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
        ?MOD_P1_TLS ->
            {ok, Sock} = gen_tcp:accept(LSocket),
            p1_tls:tcp_to_tls(Sock, TlsOpt);
        ?MOD_FAST_TLS ->
            {ok, Sock} = gen_tcp:accept(LSocket),
            fast_tls:tcp_to_tls(Sock, TlsOpt);
        ?MOD_TCP ->
            gen_tcp:accept(LSocket)
    end,

    case Resp of
        {ok, Socket} ->
            {ok, #state {socket = Socket, mod = Mod}};
        _ ->
            Resp
    end.

handshake(#state{socket = Socket, mod = Mod}) ->
    case Mod of
        ?MOD_ETLS ->
            etls:handshake(Socket);
        ?MOD_SSL ->
            ssl:ssl_accept(Socket);
        _ ->
            ok
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

getopts(#state{socket = Socket, mod = Mod}, Opt) ->
    case Mod of
        ?MOD_ETLS ->
            undefined;
        ?MOD_SSL ->
            ssl:getopts(Socket, Opt);
        ?MOD_TCP ->
            inet:getopts(Socket, Opt);
        _ ->
            #tlssock{tcpsock = TcpSocket} = Socket,
            inet:getopts(TcpSocket, Opt)
    end.

controlling_process(#state{socket = Socket, mod = Mod}, Pid) ->
    case Mod of
        ?MOD_ETLS ->
            etls:controlling_process(Socket, Pid);
        ?MOD_FAST_TLS ->
            fast_tls:controlling_process(Socket, Pid);
        ?MOD_P1_TLS ->
            p1_tls:controlling_process(Socket, Pid);
        ?MOD_SSL ->
            ssl:controlling_process(Socket, Pid);
        ?MOD_TCP ->
            gen_tcp:controlling_process(Socket, Pid)
    end.

close(#state{socket = Socket, mod = Mod}) ->
    case Mod of
        ?MOD_ETLS ->
            etls:close(Socket);
        ?MOD_FAST_TLS ->
            fast_tls:close(Socket);
        ?MOD_P1_TLS ->
            p1_tls:close(Socket);
        ?MOD_SSL ->
            ssl:close(Socket);
        ?MOD_TCP ->
            gen_tcp:close(Socket)
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

recv(#state{socket = Socket, mod = Mod} = State, Timeout) ->
    receive

        {etls, Socket, Data} ->
            {essl, State, Data};
        {ssl, Socket, Data} ->
            {essl, State, Data};
        {tcp, _TcpSocket, Data} ->
            {essl, State, read_tcp(Mod, Socket, Data)};
        {etls_closed, Socket} ->
            {essl_closed, State};
        {ssl_closed, Socket} ->
            {essl_closed, State};
        {tcp_closed, _Sock} ->
            {essl_closed, State};

        UnexpectedMsg ->
            UnexpectedMsg
    after Timeout ->
        {error, {timeout, Timeout}}
    end.

% private methods

read_tcp(?MOD_TCP, _Socket, Data) ->
    Data;
read_tcp(?MOD_FAST_TLS, Socket, TlsData) ->
    {ok, Data} = fast_tls:recv_data(Socket, TlsData),
    Data;
read_tcp(?MOD_P1_TLS, Socket, TlsData) ->
    {ok, Data} = p1_tls:recv_data(Socket, TlsData),
    Data.

get_listen_options(Mod, TlsOpt, ListenOpt) ->
    case Mod =:= ?MOD_ETLS orelse Mod =:= ?MOD_SSL of
        true ->
            ListenOpt ++ TlsOpt;
        _ ->
            ListenOpt
    end.
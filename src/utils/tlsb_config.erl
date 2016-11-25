-module(tlsb_config).
-author("silviu.caragea").

-include("tls_bench.hrl").

-export([get_config/2, get_tls_opt/2, get_server_by_port/1]).

get_config(Mod, ServerConfs) -> [
    {acceptors, tlsb_utils:lookup(acceptors, ServerConfs)},
    {listen_opt, tlsb_utils:lookup(listen_opt, ServerConfs)},
    {tls_opt, get_tls_opt(Mod, ServerConfs)},
    {tcp_opt, tlsb_utils:lookup(tcp_opt, ServerConfs)},
    {listen_port, get_port(tlsb_utils:lookup(Mod, ServerConfs))}
].

get_tls_opt(Mod, Conf) ->
    TlsOpt = tlsb_utils:lookup(tls_opt, Conf),
    Ciphers = get_ciphers(Mod, tlsb_utils:lookup(ciphers, TlsOpt)),
    TlsOpt2 = tlsb_utils:replace(ciphers, Ciphers, TlsOpt),
    TlsOpt2 ++ get_mod_tls(tlsb_utils:lookup(Mod, Conf)).

get_server_by_port(Port) ->
    {ok, Confs} = tlsb_utils:env(servers),

    try
        Fun = fun(Mod) ->
            ModPort = get_port(tlsb_utils:lookup(Mod, Confs)),
            case ModPort of
                Port ->
                    throw({found, Mod});
                _ ->
                    ok
            end
        end,
        lists:foreach(Fun, ?ALL_STACKS),
        null
    catch
        _: {found, Mod} ->
            Mod
    end.

%internals

get_mod_tls(Opt) ->
    tlsb_utils:lookup(tls_opt, Opt, []).

get_port(Opt) ->
    tlsb_utils:lookup(port, Opt).

get_ciphers(?MOD_SSL, Ciphers) ->
    Ciphers;
get_ciphers(_, Ciphers) ->
    string:join(Ciphers, ":").
-module(tlsb_config).
-author("silviu.caragea").

-include("tls_bench.hrl").

-export([get_config/2, get_tls_opt/2]).

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
    tlsb_utils:replace(ciphers, Ciphers, TlsOpt).

%internals

get_port(Opt) ->
    tlsb_utils:lookup(port, Opt).

get_ciphers(?MOD_SSL, Ciphers) ->
    Ciphers;
get_ciphers(_, Ciphers) ->
    string:join(Ciphers, ":").
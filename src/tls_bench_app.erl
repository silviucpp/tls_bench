-module(tls_bench_app).

-behaviour(application).

-include("tls_bench.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % for some reason is crashing if it's in the app.src
    % todo: investigate why
    {ok, _} = application:ensure_all_started(fast_tls),
    ok = start_servers(),
    tls_bench_sup:start_link().

stop(_State) ->
    ok.

start_servers() ->
    ok = generic_server:start(?MOD_TCP),
    ok = generic_server:start(?MOD_ETLS),
    ok = generic_server:start(?MOD_FAST_TLS),
    ok = generic_server:start(?MOD_P1_TLS),
    ok = generic_server:start(?MOD_SSL).
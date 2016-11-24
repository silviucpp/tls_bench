-module(tlsb_utils).
-author("silviu.caragea").

-export([lookup/2, lookup/3, now/0, env/1, replace/3]).

lookup(Key, List) ->
    lookup(Key, List, null).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

replace(Key, Value, List) ->
    lists:keyreplace(Key, 1, List, {Key, Value}).

now() ->
    {M, S, U} = erlang:now(),
    integer_to_binary(M * 1000000000 + S * 1000 + U).

env(Attr) ->
    application:get_env(tls_bench, Attr).
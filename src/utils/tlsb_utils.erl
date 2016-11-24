-module(tlsb_utils).
-author("silviu.caragea").

-export([
    env/1,
    lookup/2,
    lookup/3,
    replace/3,
    now/0,
    format_size/1
]).

env(Attr) ->
    application:get_env(tls_bench, Attr).

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

format_size(Size) ->
    format_size(Size, ["B","KB","MB","GB","TB","PB"]).

format_size(S, [_|[_|_] = L]) when S >= 1024 -> format_size(S/1024, L);
format_size(S, [M|_]) ->
    io_lib:format("~.2f ~s", [float(S), M]).
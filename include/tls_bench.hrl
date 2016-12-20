-author("silviu.caragea").

%stacks

-define(MOD_ETLS, etls).
-define(MOD_SSL, ssl).
-define(MOD_P1_TLS, p1_tls).
-define(MOD_FAST_TLS, fast_tls).
-define(MOD_ERLTLS, erltls).
-define(MOD_TCP, gen_tcp).

-define(ALL_STACKS, [?MOD_ETLS, ?MOD_SSL, ?MOD_P1_TLS, ?MOD_FAST_TLS, ?MOD_TCP, ?MOD_ERLTLS]).

%logs

-define(PRINT_MSG(Format, Args),
    io:format("PRINT "++Format++"~n", Args)).

-define(DEBUG_MSG(Format, Args),
    io:format("DEBUG "++Format++"~n", Args)).

-define(INFO_MSG(Format, Args),
    io:format("INFO "++Format++"~n", Args)).

-define(WARNING_MSG(Format, Args),
    io:format("WARNING "++Format++"~n", Args)).

-define(ERROR_MSG(Format, Args),
    io:format("ERROR "++Format++"~n", Args)).

-define(CRITICAL_MSG(Format, Args),
    io:format("CRITICAL "++Format++"~n", Args)).
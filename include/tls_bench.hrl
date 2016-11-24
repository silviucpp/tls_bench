-author("silviu.caragea").

%stacks

-define(MOD_ETLS, etls).
-define(MOD_SSL, ssl).
-define(MOD_P1_TLS, p1_tls).
-define(MOD_FAST_TLS, fast_tls).
-define(MOD_TCP, gen_tcp).

%logs

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-define(DEBUG_MSG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).
-module(graylog_lager_gelf_formatter).
-author("silviu.caragea").

-export([format/2, format/3]).

format(Message, Config, _Colors) ->
    format(Message, Config).

format(Message, Config) ->
    JsonPayload = jsonx:encode(get_raw_data(Message)),
    compressed(JsonPayload, graylog_lager_utils:lookup(compression, Config)).

get_raw_data(Message) ->
    [
        {version,<<"1.0">>},
        {level, lager_msg:severity_as_int(Message)},
        {short_message, lager_msg:message(Message)},
        {timestamp, graylog_lager_utils:unix_timestamp(lager_msg:timestamp(Message))},
        {host, graylog_lager_utils:hostname()} |
        get_metadata(Message)
    ].

get_metadata(Msg) ->
    get_metadata(lager_msg:metadata(Msg), []).

get_metadata([], Acc) ->
    Acc;
get_metadata([{K,V}|T], Acc) ->
    NewK = <<"_", (graylog_lager_utils:term2bin(K))/binary>>,
    get_metadata(T, [{NewK, graylog_lager_utils:term2json(V)} | Acc]).

compressed(Data, undefined) ->
    Data;
compressed(Data, gzip) ->
    zlib:gzip(Data);
compressed(Data, zlib) ->
    zlib:compress(Data).

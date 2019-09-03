-module(graylog_lager_utils).

-export([
    lookup/2,
    lookup/3,
    unix_timestamp/1,
    severity2int/1,
    term2bin/1,
    hostname/0
]).

lookup(Key, List) ->
    lookup(Key, List, undefined).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

unix_timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 + Sec + Micro / 1000000.

severity2int(debug) ->
    7;
severity2int(info) ->
    6;
severity2int(notice) ->
    5;
severity2int(warning) ->
    4;
severity2int(error) ->
    3;
severity2int(critical) ->
    2;
severity2int(alert) ->
    1;
severity2int(emergency) ->
    0;
severity2int(_) ->
    7.

term2bin(L) when is_binary(L) ->
    L;
term2bin(L) when is_list(L) ->
    iolist_to_binary(L);
term2bin(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
term2bin(P) when is_integer(P) ->
    integer_to_binary(P);
term2bin(P) when is_float(P) ->
    float_to_binary(P, [{decimals, 4}, compact]);
term2bin(P) when is_pid(P) ->
    list_to_binary(pid_to_list(P));
term2bin(Other) ->
    list_to_binary(io_lib:format("~p", [Other])).

hostname() ->
    {ok, Host} = inet:gethostname(),
    term2bin(Host).

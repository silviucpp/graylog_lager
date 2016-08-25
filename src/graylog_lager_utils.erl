-module(graylog_lager_utils).
-author("silviu.caragea").

-export([
    lookup/2,
    lookup/3,
    unix_timestamp/1,
    term2bin/1,
    term2json/1,
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

term2bin(L) when is_binary(L) ->
    L;
term2bin(L) when is_list(L) ->
    list_to_binary(L);
term2bin(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
term2bin(P) when is_pid(P) ->
    list_to_binary(pid_to_list(P));
term2bin(P) when is_integer(P) ->
    integer_to_binary(P);
term2bin(P) when is_float(P) ->
    float_to_binary(P, [{decimals, 4}, compact]);
term2bin(Other) ->
    list_to_binary(io_lib:format("~p",[Other])).

term2json(P) when is_pid(P) ->
    list_to_binary(pid_to_list(P));
term2json(P) when is_atom(P) ->
    atom_to_binary(P, latin1);
term2json(Other) ->
    Other.

hostname() ->
    {ok, Host} = inet:gethostname(),
    Host.
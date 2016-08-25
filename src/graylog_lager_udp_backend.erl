-module(graylog_lager_udp_backend).
-author("silviu.caragea").

-include_lib("lager/include/lager.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, address, port, socket, level, formatter, format_config}).

-define(DEFAULT_GELF_FORMATTER, graylog_lager_gelf_formatter).

init(Config)->
    Level        = graylog_lager_utils:lookup(level, Config, debug),
    Formatter    = graylog_lager_utils:lookup(formatter, Config, ?DEFAULT_GELF_FORMATTER),
    FormatConfig = graylog_lager_utils:lookup(format_config, Config, []),
    InetFamily   = graylog_lager_utils:lookup(inet_family, Config, inet),
    Host         = graylog_lager_utils:lookup(host, Config),
    Port         = graylog_lager_utils:lookup(port, Config),
    Name         = graylog_lager_utils:lookup(name, Config, {Host,Port}),

    validate_conf({host, Host}),
    validate_conf({port, Port}),
    validate_conf({level, Level}),
    {ok, Address} = inet:getaddr(Host, InetFamily),

    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),

    {ok, #state{
        level = lager_util:level_to_num(Level),
        name = {?MODULE, Name},
        address = Address,
        port = Port,
        socket = Socket,
        formatter = Formatter,
        format_config = FormatConfig}
    }.

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case lists:member(Level, ?LEVELS) of
        true ->
            {ok, ok, State#state{level=lager_util:level_to_num(Level)}};
        _ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, MessageInner}, #state{level=L, name = Name, formatter=Formatter, format_config=FormatConfig} = State) ->
    case lager_util:is_loggable(MessageInner, L, Name) of
        true ->
            %TODO: support chunked GELF (http://docs.graylog.org/en/2.0/pages/gelf.html)
            Msg = Formatter:format(MessageInner,FormatConfig),
            ok=gen_udp:send(State#state.socket, State#state.address, State#state.port, Msg),
            {ok, State};
        _ ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal

validate_conf({host, undefined}) ->
    throw({error, invalid_host});
validate_conf({port, Port}) ->
    case Port of
        P when P >= 1 andalso P =< 65536 ->
            true;
        _ ->
            throw({error, invalid_port})
    end;
validate_conf({inet_family, F}) when F =/= inet6 orelse F=/= inet ->
    throw({error, invalid_inet});
validate_conf({level, L}) ->
    case lists:member(L,?LEVELS) of
        true ->
            true;
        _ ->
            throw({error, invalid_level})
    end;
validate_conf(_) ->
    true.

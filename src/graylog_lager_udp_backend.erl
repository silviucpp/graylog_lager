-module(graylog_lager_udp_backend).

-include_lib("lager/include/lager.hrl").

-behaviour(gen_event).

% http://docs.graylog.org/en/3.1/pages/gelf.html

-define(CHUNK_GELF_ID, <<30,15>>).
-define(CHUNK_MAX_COUNT, 128).
-define(CHUNK_SIZE_LAN, 8154).
-define(CHUNK_SIZE_WAN, 1420).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    name,
    address,
    port,
    socket,
    level,
    formatter,
    format_config,
    chunk_size,
    shaper :: lager_shaper()
}).

-define(DEFAULT_GELF_FORMATTER, graylog_lager_gelf_formatter).

init(Config)->
    Level = graylog_lager_utils:lookup(level, Config, debug),
    Formatter = graylog_lager_utils:lookup(formatter, Config, ?DEFAULT_GELF_FORMATTER),
    FormatConfig0 = graylog_lager_utils:lookup(format_config, Config, []),
    InetFamily = graylog_lager_utils:lookup(inet_family, Config, inet),
    Host = graylog_lager_utils:lookup(host, Config),
    Port = graylog_lager_utils:lookup(port, Config),
    Name = graylog_lager_utils:lookup(name, Config, {Host, Port}),
    ChunkSize = graylog_lager_utils:lookup(chunk_size, Config, ?CHUNK_SIZE_LAN),

    Flush = graylog_lager_utils:lookup(flush_queue, Config),
    HighWaterMark = graylog_lager_utils:lookup(high_water_mark, Config),
    FlushThr = graylog_lager_utils:lookup(flush_threshold, Config, 0),

    Shaper = lager_util:maybe_flush(Flush, #lager_shaper{hwm=HighWaterMark, flush_threshold = FlushThr, id=Name}),

    validate_conf({host, Host}),
    validate_conf({port, Port}),
    validate_conf({level, Level}),
    validate_conf({chunk_size, ChunkSize}),

    FormatConfig = case graylog_lager_utils:lookup(hostname, FormatConfig0) of
        undefined ->
            [{hostname, graylog_lager_utils:hostname()}| FormatConfig0];
        _ ->
            FormatConfig0
    end,

    {ok, Address} = inet:getaddr(Host, InetFamily),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}, {sndbuf, 60000}]),

    {ok, #state{
        level = lager_util:level_to_num(Level),
        name = {?MODULE, Name},
        address = Address,
        port = Port,
        socket = Socket,
        formatter = Formatter,
        format_config = FormatConfig,
        chunk_size = ChunkSize,
        shaper = Shaper
    }}.

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

handle_event({log, MessageInner}, #state{
    level=L,
    shaper = Shaper,
    name = Name,
    formatter=Formatter,
    format_config=FormatConfig} = State) ->

    case lager_util:is_loggable(MessageInner, L, Name) of
        true ->
            case lager_util:check_hwm(Shaper) of
                {true, Drop, #lager_shaper{hwm=Hwm} = NewShaper} ->
                    case Drop > 0 of
                        true ->
                            Report = io_lib:format("graylog_lager_udp_backend dropped ~p messages in the last second that exceeded the limit of ~p messages/sec", [Drop, Hwm]),
                            write(lager_msg:new(Report, warning, [], []), Formatter, FormatConfig, State);
                        _ ->
                           ok
                    end,
                    write(MessageInner, Formatter, FormatConfig, State),
                    {ok, State#state{shaper = NewShaper}};
                {false, _, #lager_shaper{dropped=D} = NewShaper} ->
                    {ok, State#state{shaper=NewShaper#lager_shaper{dropped=D+1}}}
            end;
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

write(MessageInner, Formatter, FormatConfig, State) ->
    Msg = Formatter:format(MessageInner, FormatConfig),
    send(State, Msg, byte_size(Msg)).

send(State, Msg, MsgLength) when MsgLength =< State#state.chunk_size ->
    ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, Msg);
send(#state{chunk_size = ChunkSize} = State, Msg, MsgLength) ->
    ChunksNumber = get_chunks_number(MsgLength, ChunkSize),

    case ChunksNumber > ?CHUNK_MAX_COUNT of
        true ->
            ?INT_LOG(error, "dropped message. number of chunks exceeded: ~p total bytes: ~p", [ChunksNumber, MsgLength]);
        _ ->
            chunk_send(crypto:strong_rand_bytes(8), 0, ChunksNumber, ChunkSize, MsgLength, Msg, State)
    end.

chunk_send(_ChunkId, _SequenceNumber, _NumberOfChunks, _ChunkSize, _BodyLength, <<>>, _State) ->
    ok;
chunk_send(ChunkId, SequenceNumber, NumberOfChunks, ChunkSize, BodyLength, Body, State) ->
    RealChunkSize = erlang:min(ChunkSize, BodyLength),
    <<BodyPart:RealChunkSize/binary, Rest/binary>> = Body,
    ChunkData = <<(?CHUNK_GELF_ID)/binary, ChunkId/binary, SequenceNumber:8/integer, NumberOfChunks:8/integer, BodyPart/binary>>,
    ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, ChunkData),
    chunk_send(ChunkId, SequenceNumber + 1, NumberOfChunks, ChunkSize, BodyLength - RealChunkSize, Rest, State).

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
validate_conf({chunk_size, Value}) ->
    case Value of
        P when is_integer(P) andalso P >= ?CHUNK_SIZE_WAN andalso P =< ?CHUNK_SIZE_LAN ->
            true;
        _ ->
            throw({error, invalid_chunk_size})
    end;
validate_conf(_) ->
    true.

get_chunks_number(PayloadSize, ChunkSize) when ChunkSize < PayloadSize ->
    ChunksPerPayload = PayloadSize/ChunkSize,
    T = trunc(ChunksPerPayload),
    case ChunksPerPayload - T == 0 of
        true ->
            ChunksPerPayload;
        _ ->
            T + 1
    end.

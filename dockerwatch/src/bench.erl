-module(bench).

-export([start/0, connect/2, request/0, request/1, set_interval/1]).

start() ->
    case whereis(?MODULE) of
        undefined ->
            register(?MODULE, spawn_link(fun connector_loop/0));
        _ ->
            true
    end.

set_interval(Interval) ->
    start(),
    ?MODULE ! {set_interval, self(), Interval},
    ok.

connect(N, Rate) ->
    start(),
    ?MODULE ! {start, self(), N, Rate},
    receive
        ok -> ok
    end.

connector_loop() ->
    connector_loop(#{ alive => [], dead => []}).

connector_loop(S = #{ alive := Alive }) ->
    receive
        {set_interval, _Pid, Interval} ->
            [P ! {set_interval, Interval} || {P,_} <- Alive],
            connector_loop(S);
        {start, Pid, N, Rate} ->
            NewConns = start_connections(N, 120 * 1000, 1000 div Rate,
                                         Rate div 1000),
            Pid ! ok,
            connector_loop(S#{ alive := lists:flatten([NewConns|Alive]) })
    end.

start_connections(PoolSize, Interval, 0, Rate) ->
    start_connections(PoolSize, Interval, 1, Rate);
start_connections(PoolSize, Interval, Sleep, 0) ->
    start_connections(PoolSize, Interval, Sleep, 1);
start_connections(0, _Interval, _Sleep, _Rate) ->
    [];
start_connections(PoolSize, Interval, Sleep, Rate) when PoolSize < Rate ->
         timer:sleep(Sleep),
    [ spawn_monitor(fun() -> init(Interval) end)
     | start_connections(PoolSize-1, Interval, Sleep, Rate)];
start_connections(PoolSize, Interval, Sleep, Rate) ->
    timer:sleep(Sleep),
    [[spawn_monitor(fun() -> init(Interval) end) || _ <- lists:seq(1,Rate)]
     | start_connections(PoolSize-Rate, Interval, Sleep, Rate)].

init(Interval) ->
    loop(do_request(undefined), Interval, []).

loop(S, Interval, Latency) ->

    SleepTime = rand:uniform(Interval),
    erlang:garbage_collect(),
    receive
        {set_interval, NewInterval} ->
            loop(S, NewInterval, Latency);
        {stop, Pid} -> Pid ! Latency
    after SleepTime ->
            NewS = do_request(S),
            loop(NewS, Interval, Latency)
    end.

do_request(S) ->
    case request(S) of
        {NewS, ok, _T} ->
            ok;
        {NewS, error, Reason} ->
            case Reason of
                timeout ->
                    io:format("T");
                timedout ->
                    io:format("T");
                eaddrinuse ->
                    io:format("E"),
                    exit(normal);
                closed ->
                    %% io:format("C"),
                    ok;
                _ ->
                    io:format("~p",[Reason])
            end
    end,
    NewS.

request() ->
    request(undefined).
request(undefined) ->
    Ip = rand:uniform(length(dockerwatch_sup:ips())),
%    {_, {_, _, Ip, _}} = hd(dockerwatch_sup:ips()),
    RemoteIp = {172,19,Ip,2},
    LocalIp  = {172,19,Ip,3},
    case gen_tcp:connect(RemoteIp, 8080,
                         [{sndbuf, 512},{recbuf, 512},{buffer,512},
                          {ip, LocalIp}, binary, {active, false}]) of
        {ok, S} ->
            request(S);
        {error, R} ->
            {undefined, error, R}
    end;
request(S) ->

    Data = <<"GET /cnt HTTP/1.1\r\n"
             "Host: apollo:8080\r\n"
             "Connection: keep-alive\r\n"
             "User-Agent: curl/7.47.0\r\n"
             "Accept: application/json\r\n\r\n">>,

    T0 = erlang:monotonic_time(microsecond),
    case gen_tcp:send(S, Data) of
        ok ->
            case receive_until(S, undefined, []) of
                ok ->
                    T1 = erlang:monotonic_time(microsecond),
                    {S, ok, T1 - T0};
                {error, Reason} ->
                    {undefined, error, Reason}
            end;
        {error, Reason} ->
            {undefined, error, Reason}
    end.


receive_until(S, Len, D) ->
    case gen_tcp:recv(S, 0) of
        {ok, Data} ->
            BinD = iolist_to_binary([D|Data]),
            decode(S, BinD, Len);
        {error, Reason} ->
            {error, Reason}
    end.

decode(S, Data, Len) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {more, _} ->
            receive_until(S, Len, Data);
        {ok, Packet, Rest} ->
            decode(S, Packet, Rest, Len);
        {error, R} ->
            {error, R}
    end.

decode(S, http_eoh, Rest, Len) ->
    if size(Rest) < Len ->
            case gen_tcp:recv(S, 0) of
                {ok, Data} ->
                    decode(S, http_eoh, iolist_to_binary([Data|Rest]), Len);
                {error, R} ->
                    {error, R}
            end;
       size(Rest) =:= Len ->
            ok
    end;
decode(S, {http_header,_,'Content-Length',_,L}, Rest, undefined) ->
    decode(S, Rest, binary_to_integer(L));
decode(S, {http_response,_,_,_}, Rest, L) ->
    decode(S, Rest, L);
decode(S, {http_error,_}, Rest, L) ->
    decode(S, Rest, L);
decode(S, {http_header,_,_,_,_}, Rest, L) ->
    decode(S, Rest, L).

%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_sup.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_sup).
-behaviour(supervisor).

-export([start_link/0,init/1, spawn_info/0, info/0, ips/0]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    CertsDir = "/etc/ssl/certs/",

    Dispatch = cowboy_router:compile([
	    {'_', [{"/[:counter_name]", dockerwatch_handler, []}]}
	]),

    HTTPS = ranch:child_spec(
              cowboy_https, 100, ranch_ssl,
              [{port, 8443},
               {cacertfile, filename:join(CertsDir, "dockerwatch-ca.pem")},
               {certfile, filename:join(CertsDir, "dockerwatch-server.pem")},
               {keyfile, filename:join(CertsDir, "dockerwatch-server.key")}],
              cowboy_protocol,
              [{env, [{dispatch, Dispatch}]}]),
%start_listener(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts)

    [ranch:start_listener(
       Name,
       400, ranch_tcp,
       [{port, 8080},{backlog,400},
        {ip, Ip},
        {sndbuf, 512},{recbuf, 512},{buffer,512},
        {max_connections, infinity}],
       cowboy_protocol,
       [{max_keepalive,5000000},
        {timeout, 120000},
        {env, [{dispatch, Dispatch}]}])
     || {Name, Ip} <- ips()],

    spawn_info(),

    %% HTTP = [ranch:child_spec(
    %%           list_to_atom("cowboy_http_" ++ integer_to_list(Prt)),
    %%           40, ranch_tcp,
    %%           [{port, 8080 + Prt},{backlog,40},
    %%            {max_connections, infinity}],
    %%           cowboy_protocol,
    %%           [{env, [{dispatch, Dispatch}]}])
    %%         || Prt <- lists:seq(0, 100)],

    Counter = {dockerwatch, {dockerwatch, start_link, []},
               permanent, 5000, worker, [dockerwatch]},

    Procs = lists:flatten([Counter, HTTPS]),

    {ok, {{one_for_one, 10, 10}, Procs}}.


spawn_info() ->
    spawn(fun F() ->
                  io:format("~p~n",[dockerwatch_sup:info()]),
                  timer:sleep(10000), F()
          end).

info() ->
    V = lists:map(fun({_, Props}) ->
                          proplists:get_value(active_connections, Props) end,
                  ranch:info()),
    {lists:sum(V), lists:max(V), lists:min(V)}.

ips() ->
    case get(ips) of
        undefined ->
            {ok, IFs} = inet:getifaddrs(),
            IPs = [{list_to_atom("dockerwatch_" ++ Eth), proplists:get_value(addr, Vs)}
                   || {"eth" ++ _ = Eth, Vs} <- IFs],
            put(ips, IPs),
            IPs;
        IPs ->
            IPs
    end.

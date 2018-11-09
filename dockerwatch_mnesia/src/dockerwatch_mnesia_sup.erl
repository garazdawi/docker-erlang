%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_sup.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_mnesia_sup).
-behaviour(supervisor).

-export([start_link/0,init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    PromConfig =
        #{ env => #{ dispatch =>
                         cowboy_router:compile(
                           [{'_', [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}]) }
         },

    Prometheus = ranch:child_spec(
                   cowboy_prometheus, 100, ranch_tcp,
                   [{port, 9000}],
                   cowboy_clear,
                   PromConfig),

    Procs = [Prometheus],

    {ok, {{one_for_one, 10, 10}, Procs}}.

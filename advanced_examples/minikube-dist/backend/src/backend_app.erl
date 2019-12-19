%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    backend_app.erl
%% Author:  Lukas Larsson
%% Created: 2019-12-13
%%

-module(backend_app).
-behaviour(application).

-export([start/2,stop/1]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").

%% API.

start(_Type, _Args) ->
    connect_db_nodes(),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.

%% This will call erl_epmd_dns_srv:names(SRV) and then do net_adm:ping to
%% all nodes in the world list.
connect_db_nodes() ->
    {ok, SVC} = application:get_env(backend, service),
    net_kernel:world_list(SRV).

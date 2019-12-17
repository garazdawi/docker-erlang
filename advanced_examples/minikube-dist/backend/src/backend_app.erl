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

%% API.

start(_Type, _Args) ->
    os:set_signal(sigchld,ignore),
    connect_db_nodes(),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.

%% We lookup the k8s service through dns and connect to all backend nodes
connect_db_nodes() ->
    {ok, SVC} = application:get_env(backend, service),
    {ok,#hostent{ h_name = FQDN }} = inet:gethostbyname(SVC),
    Nodes = [list_to_atom("dockerwatch@"++Host) ||
                {_Prio,_Weight,_Port,Host} <- inet_res:lookup(FQDN,any,srv)],
    [net_adm:ping(N) || N <- Nodes].

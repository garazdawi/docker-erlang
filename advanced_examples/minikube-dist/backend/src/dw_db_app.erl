%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    backend_app.erl
%% Author:  Lukas Larsson
%% Created: 2019-12-13
%%

-module(dw_db_app).
-behaviour(application).

-export([start/2,stop/1]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").

%% API.

start(_Type, _Args) ->
    os:set_signal(sigchld, ignore),
    connect_db_nodes(),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.

%% We lookup the k8s service through dns and connect to all backend nodes
connect_db_nodes() ->
    FQDN = get_service(),
    Nodes = [list_to_atom("dockerwatch@"++Host) ||
                {_Prio,_Weight,_Port,Host} <- inet_res:lookup(FQDN,any,srv)],
    [net_adm:ping(N) || N <- Nodes].

get_service() ->
    {ok, SVC} = application:get_env(dw_db, service),
    get_service(SVC,0,10).
get_service(SVC,N,M) ->
    case inet:gethostbyname(SVC) of
        {ok,#hostent{ h_name = FQDN }} ->
            FQDN;
        {error,nxdomain} when N =/= M ->
            ?LOG_INFO("Failed to get service, sleeping and then retrying"),
            timer:sleep(N*N * 100),
            get_service(SVC,N+1,M);
        Res ->
            Res
    end.

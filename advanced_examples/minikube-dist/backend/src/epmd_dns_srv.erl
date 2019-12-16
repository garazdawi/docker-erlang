%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(epmd_dns_srv).

-export([start_link/0, register_node/2, register_node/3, address_please/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/inet.hrl").
%% API.

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet_tcp).
register_node(_Name, _Port, _Driver) ->
    {ok, 0}.

address_please(_Name, Host, Family) ->
    case inet:gethostbyname(Host, Family) of
        {ok,#hostent{ h_name = FQDN, h_addr_list = [Addr|_] }} ->
            Port = case inet_res:lookup(FQDN,any,srv) of
                       [{_Prio,_Weight,Prt,FQDN}] ->
                           {ok,Addr,Prt,5};
                       [] ->
                           {ok, Port} = application:get_env(kernel,inet_tcp_dist_port),
                           Port
                   end,
            {ok,Addr,Port,5}
    end.

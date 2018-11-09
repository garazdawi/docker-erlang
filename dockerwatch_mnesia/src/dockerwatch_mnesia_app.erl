%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_mnesia_app).
-behaviour(application).

-export([start/2,stop/1]).
%% API.

start(_Type, _Args) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(dockerwatch,
                        [{disc_copies,[node()]},
                         {ram_copies,['dockerwatch@dockerwatch.default.svc.cluster.local']}]),
    dockerwatch_mnesia_sup:start_link().

stop(_State) ->
    ok.

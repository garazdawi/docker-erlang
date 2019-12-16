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

%% API.

start(_Type, _Args) ->
    os:set_signal(sigchld,ignore),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.

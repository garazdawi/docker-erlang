%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_handler.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch).

-export([all/0, create/1, get/1, increment/2, decrement/2]).

-type counter() :: binary().

-spec all() -> [counter()].
all() ->
    rpc(?MODULE,?FUNCTION_NAME,[]).

-spec create(counter()) -> ok | already_exists.
create(CounterName) ->
    rpc(?MODULE,?FUNCTION_NAME,[CounterName]).

-spec get(counter()) -> integer().
get(CounterName) ->
    rpc(?MODULE,?FUNCTION_NAME,[CounterName]).

-spec increment(counter(), integer()) -> ok.
increment(CounterName, Howmuch) ->
    rpc(?MODULE,?FUNCTION_NAME,[CounterName, Howmuch]).

-spec decrement(counter(), integer()) -> ok.
decrement(CounterName, Howmuch) ->
    rpc(?MODULE,?FUNCTION_NAME,[CounterName, Howmuch]).

rpc(M,F,A) ->
    rpc:call(get_node_from_srv(),M,F,A).

get_node_from_srv() ->
    {ok, SVC} = application:get_env(dockerwatch,backend),
    Nodes = inet_res:lookup(SVC,in,srv),
    NodeSum = lists:foldl(fun({_,Weight,_,_},Acc) ->
                                  Weight + Acc
                          end,0,Nodes),
    Rand = rand:uniform(NodeSum),
    Host = lists:foldl(fun({_,Weight,_,Host},Acc) when (Acc - Weight) =< 0 ->
                               Host;
                          ({_,Weight,_,_Host},Acc) when is_integer(Acc) ->
                               Acc - Weight;
                          (_,Host) ->
                               Host
                       end,Rand,Nodes),
    list_to_atom("dockerwatch@"++Host).

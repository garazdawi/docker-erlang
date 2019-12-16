%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch).

-export([start_link/0, all/0, create/1, get/1, increment/2, decrement/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").

-type counter() :: binary().

-spec start_link() -> {ok, pid()}.
start_link() ->
    case get_extra_nodes() of
        [] ->
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            mnesia:create_table(?MODULE,
                                [{disc_copies,[node()]},
                                 {ram_copies,[]}]);
        ExtraNodes ->
            mnesia:change_config(extra_db_nodes,ExtraNodes),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            mnesia:add_table_copy(?MODULE, node(), disc_copies)
    end,

    ok = mnesia:wait_for_tables([?MODULE], 15000),
    ignore.

get_extra_nodes() ->
    {ok, SVC} = application:get_env(backend, service),
    {ok,#hostent{ h_name = FQDN }} = inet:gethostbyname(SVC),
    [list_to_atom("dockerwatch@"++Host) ||
        {_Prio,_Weight,_Port,Host} <- inet_res:lookup(FQDN,any,srv)] -- [node()].


-spec all() -> [counter()].
all() ->
    case mnesia:transaction(
           fun() ->
                   mnesia:select(?MODULE, [{{?MODULE,'$1','_'},[],['$1']}])
           end) of
        {atomic, Res} ->
            Res
    end.

-spec create(counter()) -> ok | already_exists.
create(CounterName) ->
    case mnesia:transaction(
           fun() ->
                   case mnesia:read(?MODULE, CounterName) of
                       [] ->
                           mnesia:write({?MODULE,CounterName,0}),
                           ok;
                       _Else ->
                           already_exists
                   end
           end) of
        {atomic, Res} ->
            Res
    end.

-spec get(counter()) -> integer().
get(CounterName) ->
    case mnesia:transaction(
           fun() ->
                   mnesia:read(?MODULE, CounterName)
           end) of
        {atomic,[{?MODULE, _, Cnt}]} ->
            Cnt
    end.

-spec increment(counter(), integer()) -> ok.
increment(CounterName, Howmuch) ->
    case mnesia:transaction(
           fun() ->
                   [{?MODULE, _, Cnt}] = mnesia:read(?MODULE, CounterName),
                   mnesia:write({?MODULE, CounterName, Cnt + Howmuch})
           end) of
        {atomic, ok} ->
            ok
    end.

-spec decrement(counter(), integer()) -> ok.
decrement(CounterName, Howmuch) ->
    increment(CounterName, -1 * Howmuch).

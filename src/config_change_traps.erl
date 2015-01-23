-module(config_change_traps).
-behaviour(gen_event).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

init([]) ->
    {ok, []}.

handle_event({Address, [1,3,6,1,6,3,1,1,5,1], _SnmpInfo}, []) ->
    io:format("~p cold restart, check for config changes~n", [Address]),
    {ok, []};
handle_event({Address, [1,3,6,1,4,1,9,9,43,2,0,2], _SnmpInfo}, []) ->
    io:format("~p running config change [CISCO], check for config changes~n", [Address]),
    {ok, []};
handle_event({Address, TrapOid, _SnmpInfo}, []) ->
    io:format("uninteresting trap ~p from ~p~n", [TrapOid, Address]),
    {ok, []}.

handle_info(_Msg, State) ->
    {ok , State}.

handle_call(_Req, State) ->
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

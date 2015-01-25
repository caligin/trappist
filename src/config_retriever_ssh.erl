-module(config_retriever_ssh).
-behaviour(gen_fsm).

%% API.
-export([start/2]).

%% gen_fsm.
-export([init/1]).
-export([unconnected/2]).
-export([connected/2]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(SSH_OPTS, [
    {silently_accept_hosts, true},
    {user_interaction, false},
    {quiet_mode, true}
    ]).

-record(state, {
    target,
    result_cb, %% {module, function}
    conn_ref,
    chan_id,
    configuration = []
}).

%% API.

-spec start(_T, _CB) -> {ok, pid()}.
start(Target, {_CallbackModule, _CallbackFun} = CB) ->
    gen_fsm:start(?MODULE, [Target, CB], []).

%% gen_fsm.

init([Target, CB]) ->
    gen_fsm:send_event(self(), connect),
    {ok, unconnected, #state{ target = Target, result_cb = CB}}.

unconnected(connect, State) ->
    {ConnRef, ChanId} = connect(State#state.target),
    gen_fsm:send_event(self(), ask_config),
    {next_state, connected, State#state{ conn_ref = ConnRef, chan_id = ChanId}}.

connected(ask_config, State) ->
    %% might want a vendor detection state before asking conf
    %% disabling pagination might be another step in the proto if a response is expected
    ok = disable_pagination_cisco(State#state.conn_ref, State#state.chan_id),
    ok = send_configuration_request_cisco(State#state.conn_ref, State#state.chan_id),
    {next_state, receiving, State}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.

handle_info({data, ChanId, _Type, ReceivedData}, receiving, StateData = #state{ configuration = Acc, chan_id = ChanId}) ->
    {next_state, receiving, StateData#state{ configuration = [ReceivedData | Acc] }};
handle_info({eof, ChanId}, receiving, StateData = #state{ target = Target, configuration = Acc, chan_id = ChanId, result_cb = {CallbackModule, CallbackFun}}) ->
    CallbackModule:CallbackFun(Target, Acc),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{ conn_ref = ConnRef, chan_id = ChanId}) ->
    ssh_connection:close(ConnRef, ChanId),
    ssh:close(ConnRef),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



connect(Target) ->
    User = application:get_env(trappist, retriever_user, "ackbar"),
    Password = application:get_env(trappist, retriever_password, "trap"),
    {ok, ConnRef} = ssh:connect(binary_to_list(Target), 22, [{user, User} | [{password, Password} | ?SSH_OPTS]], infinity),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, infinity),
    {ConnRef, ChanId}.

disable_pagination_cisco(ConnRef, ChanId) ->
    ssh_connection:send(ConnRef, ChanId, <<"terminal-length 0\n">>).

send_configuration_request_cisco(ConnRef, ChanId) ->
    ssh_connection:send(ConnRef, ChanId, <<"show run\n">>).


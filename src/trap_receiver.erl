-module(trap_receiver).
-behaviour(snmpm_user).
-export([
    start_link/0,
    add_handler/2
    ]).
-export([
    handle_error/3,
    handle_agent/5,
    handle_pdu/4,
    handle_trap/3,
    handle_inform/3,
    handle_report/3,
    handle_invalid_result/2
    ]).

-define(TRAP_OID_OID, [1,3,6,1,6,3,1,1,4,1,0]).

start_link() ->
    ok = snmpm:start_link(manager_opts()),
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

manager_opts() ->
    [
        {config, [
            {dir, "snmp/manager/conf/"},
            {db_dir, "snmp/manager/db/"}
            ]},
        {def_user_mod, ?MODULE}
    ].

handle_error(ReqId, Reason, UserData) ->
    io:format("error ~p ~p ~p ~n", [ReqId, Reason, UserData]).

handle_agent(Domain, Addr, trap, {_, _, Payload} = SnmpInfo, UserData) ->
    io:format("it's a trap (unknown agent)! ~p ~p ~p ~p ~n",[Domain, Addr, SnmpInfo, UserData]),
    {TrapOid, Varbinds} = extract_data(Payload),
    gen_event:notify(?MODULE, {Domain, TrapOid, Varbinds}),
    ignore;
handle_agent(Domain, Addr, Type, SnmpInfo, UserData) ->
    io:format("agent ~p ~p ~p ~p ~p ~n",[Domain, Addr, Type, SnmpInfo, UserData]),
    ignore.

handle_pdu(_TargetName, _ReqId, _SnmpPduInfo, _UserData) ->
    io:format("pdu").

handle_trap(TargetName, SnmpTrapInfo, UserData) ->
    io:format("it's a trap (known agent)! ~p ~p ~p ~n",[TargetName, SnmpTrapInfo, UserData]),
    ignore.

handle_inform(TargetName, SnmpInformInfo, UserData) -> 
    io:format("inform ~p ~p ~p ~n",[TargetName, SnmpInformInfo, UserData]),
    ignore.

handle_report(TargetName, SnmpReportInfo, UserData) ->
    io:format("report ~p ~p ~p ~n",[TargetName, SnmpReportInfo, UserData]).

handle_invalid_result(IN, OUT) ->
    io:format("BABUM ~p ~p ~n",[IN, OUT]).

extract_data(Payload) ->
    extract_data(Payload, [], []).

extract_data([{varbind, ?TRAP_OID_OID, 'OBJECT IDENTIFIER', Value, _Index} | Others], Varbinds, []) ->
    extract_data(Others, Varbinds, [Value]);
extract_data([{varbind, Oid, Type, Value, _Index} | Others], Varbinds, TrapOid) ->
    extract_data(Others, [{Oid, Type, Value} | Varbinds], TrapOid);
extract_data([], Varbinds, [TrapOid]) ->
    {TrapOid, Varbinds}.

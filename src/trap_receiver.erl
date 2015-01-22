-module(trap_receiver).
-behaviour(snmpm_user).
-export([
    start/0,
    manager_opts/0
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

start() ->
    ok = snmpm:start_link(manager_opts()).
    %, 
    %snmpm:register_user("trap_receiver", ?MODULE, undefined).

manager_opts() ->
    [
        {config, [
            {dir, "snmp/manager/conf/"},
            {db_dir, "snmp/manager/db/"}
            ]},
        {def_user_mod, ?MODULE}
    ].

%-spec handle_error(ReqId, Reason, UserData) -> void().
handle_error(ReqId, Reason, UserData) ->
    io:format("error ~p ~p ~p ~n", [ReqId, Reason, UserData]).

%-spec handle_agent(Domain, Addr, pdu | trap | report | inform, SnmpInfo, UserData) -> Reply.
handle_agent(Domain, Addr, trap, SnmpInfo, UserData) ->
    io:format("it's a trap (unknown agent)! ~p ~p ~p ~p ~n",[Domain, Addr, SnmpInfo, UserData]),
    ignore;
handle_agent(Domain, Addr, Type, SnmpInfo, UserData) ->
    io:format("agent ~p ~p ~p ~p ~p ~n",[Domain, Addr, Type, SnmpInfo, UserData]),
    ignore.

%-spec handle_pdu(TargetName, ReqId, SnmpPduInfo, UserData) -> void().
handle_pdu(_TargetName, _ReqId, _SnmpPduInfo, _UserData) ->
    io:format("pdu").

%-spec handle_trap(TargetName, SnmpTrapInfo, UserData) -> Reply.
handle_trap(TargetName, SnmpTrapInfo, UserData) ->
    io:format("it's a trap (known agent)! ~p ~p ~p ~n",[TargetName, SnmpTrapInfo, UserData]),
    ignore.

%-spec handle_inform(TargetName, SnmpInformInfo, UserData) -> Reply.
handle_inform(TargetName, SnmpInformInfo, UserData) -> 
    io:format("inform ~p ~p ~p ~n",[TargetName, SnmpInformInfo, UserData]),
    ignore.

%-spec handle_report(TargetName, SnmpReportInfo, UserData) -> Reply.
handle_report(TargetName, SnmpReportInfo, UserData) ->
    io:format("report ~p ~p ~p ~n",[TargetName, SnmpReportInfo, UserData]).

%-spec handle_invalid_result(IN, OUT) -> void().
handle_invalid_result(IN, OUT) ->
    io:format("BABUM ~p ~p ~n",[IN, OUT]).
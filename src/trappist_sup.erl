-module(trappist_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        {trap_receiver, {trap_receiver, start_link, []}, permanent, brutal_kill, worker, [trap_receiver]}
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

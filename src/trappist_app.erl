-module(trappist_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Supervisor} = trappist_sup:start_link(),
    trap_receiver:add_handler(config_change_traps, []),
    {ok, Supervisor}.

stop(_State) ->
	ok.

-module(super).
-behaviour(supervisor).

%%Export funktioner
-export([start_link/1]).
-export([init/1]).

start_link(Type)->
    supervisor:start_link({local,?MODULE},?MODULE, Type).



init(one_for_one)->
    init(one_for_one,1,30).





init({Re, MaxRestarts, MaxTime})->
    {ok,{{Re, MaxRestars, MaxTime},

%%{id, startfunc, restart, shutdown, type, modules} 

	 [{ID,
	   {?MODULE, start_link, []},
	   permanent,1000,worker,[?MODULE]}]}}.

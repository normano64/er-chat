%%Authors: Sam Rönnlund (sam.ronnlund.6971@student.uu.se) & Tomas Wallin(tomas.wallin.XXXX@student.uu.se)
%%
%%


-module(command_sup).
-behaviour(supervisor).

%% exportfunctions
-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Res)->
    io:format("Starting command_sup (~w)~n", [self()]),
    RestartStrategy = {one_for_one, 3, 60},
    Command = {command,
		{command, start_link, []},
		permanent, 1000, worker, [command]},
    Child = [Command],
    {ok, {RestartStrategy, Child}}.
    
    

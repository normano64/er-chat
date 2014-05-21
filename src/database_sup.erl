%%Authors: Sam Rönnlund (sam.ronnlund.6971@student.uu.se) & Tomas Wallin(tomas.wallin.XXXX@student.uu.se)
%%
%%


-module(database_sup).
-behaviour(supervisor).

%% exportfunctions
-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Res)->
    io:format("Starting database_sup (~w)~n", [self()]),
    RestartStrategy = {one_for_one, 3, 60},
    Database = {database,
		{database, start_link, []},
		permanent, 1000, worker, [database]},
    Child = [Database],
    {ok, {RestartStrategy, Child}}.
    
    

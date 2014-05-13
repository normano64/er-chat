%% Authors: Sam Rönnlund (sam.ronnlund.6971@student.uu.se) & Tomas Wallin(tomas.wallin.XXXX@student.uu.se).
%%
%%
%%
%%
%%

-module(server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Res)->
    io:format("starting server_sup (~w) ~n", [self()]),
    RestartStrategy = {one_for_one, 3, 60},
    Server = {server, 
	      {server, start_link, []},
	      permanent, 1000, worker, [server]},
    Child = [Server],
    {ok, {RestartStrategy, Child}}.


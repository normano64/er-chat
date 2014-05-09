-module(server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Res)->
    RestartStrategy = {one_for_one, 3, 60},
    Server = {server, 
	      {server, start_link, []},
	      permanent, 1000, worker, [server]},
    Children = [Server],
    {ok, {RestartStrategy, Children}}.


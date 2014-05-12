-module(super).
-behaviour(supervisor).

%%Export funktioner
-export([start_link/0, init/1]).


start_link()->
    supervisor:start_link({local,?MODULE},?MODULE, []).



init(_Res)->
    RestartStrategy = {one_for_one,3,60},
   
    Server_sup =  {server_sup,
		   {server_sup, start_link, []},
		   permanent, infinity, supervisor, [server_sup]},
    %% Database_sup = {database_sup,
    %% 		    {database_sup, start_link, []},
    %% 		    permanent, infinity, supervisor,[database_sup]},
    %% Parser_sup = {parser_sup, 
    %% 		  {parser_sup, start_link, []},
    %% 		  permanent, infinity, supervisor, [parser_sup]},
    %% Command_sup = {command_sup, 
    %% 		   {command_sup, start_link, []},
    %% 		   permanent, infinity, supervisor, [command_sup]},
    
    Children = [Server_sup],
    {ok, {RestartStrategy, Children}}.
    


%%whereis = returns pid.
%%shutdown()->
 %%   exit(whereis(?MODULE), shutdown).


  



    %% {ok,{{Re, MaxRestars, MaxTime},
    %% 	 [{ID,{server, start_link, []},
    %% 	   permanent,1000,worker,[server]}]}}.


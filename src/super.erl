%% Authors: Sam Rönnlund(sam.ronnlund.6971@student.uu.se) & Tomas Wallin(tomas.wallin.XXXX@student.uu.se).
%%
%%
%%
%%
%%



-module(super).
-behaviour(supervisor).

%%Export funktioner
-export([start_link/0, init/1, shutdown/0]).


start_link()->
    supervisor:start_link({local,?MODULE},?MODULE, []).



init(_Res)->
    io:format("Starting super (~w) ~n",[self()]),
    RestartStrategy = {one_for_one,3,60},
   
    Server_sup =  {server_sup,
		   {server_sup, start_link, []},
		   permanent, infinity, supervisor, [server_sup]},
    Database_sup = {database_sup,
		    {database_sup, start_link, []},
		    permanent, infinity, supervisor,[database_sup]},
    Parser_sup = {parser_sup, 
     		  {parser_sup, start_link, []},
     		  permanent, infinity, supervisor, [parser_sup]},
    Command_sup = {command_sup, 
     		   {command_sup, start_link, []},
     		   permanent, infinity, supervisor, [command_sup]},
    
    ChildSpecifications = [Server_sup, Database_sup, Parser_sup, Command_sup],
    {ok, {RestartStrategy, ChildSpecifications}}.
    

shutdown()->
   exit(whereis(?MODULE), shutdown).



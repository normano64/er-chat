-module(server_sup).
-behaviour(supervisor).
-export([start/0, init/1, start_socket/0]).

start()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([])->
    {ok, Listen} = gen_tcp:listen(6667, [binary,{packet, 0}, {active, false}, {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    %%spawn_link(fun empty_listeners/0),
    io:format("~p connected~n", [Socket]),
    
    {ok,List} = inet:getif(),
    {ServerIP,_,_} = lists:nth(1, List),
    {ok,{hostent,ServerHostent,_,_,_,_}} = inet:gethostbyaddr(ServerIP),
    Host = {list_to_binary(inet:ntoa(ServerIP)),list_to_binary(ServerHostent)},

    %% Childspec
    io:format("innan childspec~n"),
    {ok,{{simple_one_for_one, 60, 3600},
	[{socket,
	 {server2,start,[Socket,Host]},
	 temporary,1000,worker,[server]}
	]}}.

start_socket()->
    supervisor:start_child(?MODULE,[]).

%%empty_listeners() ->
  %%  [start_socket() || _ <- lists:seq(1,20)],
   %% ok.


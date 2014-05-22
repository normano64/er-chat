-module(server2).
-compile(export_all).
-behaviour(gen_server).
-export([init/1,handle_cast/2,handle_call/3,handle_info/2,start/1,terminate/2,code_change/3]).
start(List)->
    io:format("INNAN INIT :S:S:S:S~n"),
    gen_server:start_link(?MODULE,List,[]).

init(List)->
    [Socket,Host] = List,
    _Pid = self(),
    io:format("Innan handle cast~n"),
    gen_server:cast({Socket,Host},accept),
    {ok,{Socket,Host}}.

%% Asynchronus
handle_cast(accept,{Socket,Host})->
    %% acceptSocket for user, Socket for server
    io:format("HANDLE call accept~n"),
    {ok, AcceptSocket} = gen_tcp:accept(Socket),

    UserPid = spawn_link(fun()-> commands:loop_user(Host,Socket) end),
    OtherPid = spawn_link(fun()-> commands:loop_other(Host,Socket, UserPid) end),
    ParserPid = spawn_link(fun()-> parser:loop(UserPid,OtherPid) end),

    supervisor:start_socket(),
    Receiver = gen_tcp:recv(AcceptSocket,0,1000),
    gen_server:cast(Receiver,{AcceptSocket,Host,ParserPid,0}),
    {noreply,state};
handle_cast({ok,Message},{AcceptSocket,Host,ParsePid,_Timeout}) ->
    io:format("~p: ~p~n", [AcceptSocket, Message]),
    CommandList = binary:split(Message,<<"\n">>, [trim,global]),
    send_messages(ParsePid, CommandList),
    Receiver = gen_tcp:recv(AcceptSocket,0,1000),
    gen_server:cast(Receiver,{AcceptSocket,Host,ParsePid,0});
handle_cast({error,timeout},{AcceptSocket,Host,ParsePid,Timeout}) ->
    case Timeout of
	1 ->
	    io:format("~p closed, reason: timeout~n",[AcceptSocket]),
	    commands:quit(<<"client timeout">>, Host, AcceptSocket);
	_ ->
	    commands:ping(Host, AcceptSocket),
	    Receiver = gen_tcp:recv(AcceptSocket,0,1000),
	    gen_server:cast(Receiver,{AcceptSocket,Host,ParsePid,0})
	end;
handle_cast({error,Reason},{Socket,Host,_ParsePid,_Timeout}) ->
    io:format("~p closed, reason: ~p~n", [Socket, Reason]),
    commands:quit(<<"crash">>, Host, Socket),
    {stop,Reason}.

%% Synchronus
handle_call(_,_,_)->
    ok.
%% handles stuff like timeout
handle_info(_,_)->
    ok.

%% terminates server
terminate(_,_)->
    ok.

%% change code
code_change(_,_,_)->
    ok.

send_messages(_,[])->
    [];
send_messages(ParserPid,[H|T]) ->
    ParserPid ! {ok, H},
    send_messages(ParserPid,T).

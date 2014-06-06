-module(server2).
%%-compile(export_all).
-behaviour(gen_server).
-export([init/1,handle_cast/2,handle_call/3,handle_info/2,start_link/2,terminate/2,code_change/3]).
-define(TIMEOUT,60000).

start_link(Socket,Host)->
    List = [Socket,Host],
    gen_server:start_link({local,whatever},?MODULE,List,[]).

init(List)->
    [Socket,Host] = List,
    Pid = self(),
    gen_server:cast(Pid,accept),
    {ok,{Socket,Host}}.

%% Asynchronus
handle_cast(accept,{Socket,Host})->
    %% acceptSocket for user, Socket for server
    %%{ok, AcceptSocket} = gen_tcp:accept(Socket),

    UserPid = spawn_link(fun()-> commands:loop_user(Host,Socket) end),
    OtherPid = spawn_link(fun()-> commands:loop_other(Host,Socket, UserPid) end),
    ParserPid = spawn_link(fun()-> parser:loop(UserPid,OtherPid) end),

    Pid = self(),
    gen_server:cast(Pid,recv),
    {noreply,{Socket,Host,ParserPid,0,Pid}};

handle_cast(recv,{Socket,Host,ParserPid,Timeout,Pid}) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Message} ->
            io:format("~p: ~p~n", [Socket, Message]),
            case Message of
                <<"PRIVMSG OP :DIE\r\n">> ->
                    exit("apa");
                _ ->
                    CommandList = binary:split(Message,<<"\n">>, [trim,global]),
                    send_messages(ParserPid, CommandList),
                    gen_server:cast(Pid,recv),
                    {noreply,{Socket,Host,ParserPid,0,Pid}}
            end;
        {error, timeout} ->
            case Timeout of
                1 ->
		    io:format("~p closed, reason: timeout~n",[Socket]),
		    commands:quit(<<"client timeout">>, Host, Socket);
                _ ->
                    commands:ping(Host, Socket),
                    gen_server:cast(Pid,recv),
                    {noreply,{Socket,Host,ParserPid,1,Pid}}
            end;
        {error, Reason} ->
            io:format("~p closed, reason: ~p~n", [Socket, Reason]),
            commands:quit(<<"crash">>, Host, Socket),
            exit(Reason)
    end.

%% handle_cast({ok,Message},{AcceptSocket,Host,ParsePid,_Timeout}) ->
%%     io:format("~p: ~p~n", [AcceptSocket, Message]),
%%     CommandList = binary:split(Message,<<"\n">>, [trim,global]),
%%     send_messages(ParsePid, CommandList),
%%     Pid = self(),
%%     Receiver = gen_tcp:recv(AcceptSocket,0,1000),
%%     gen_server:cast(Pid,Receiver),
%%     {ok,{AcceptSocket,Host,ParsePid,0}};
%% handle_cast({error,timeout},{AcceptSocket,Host,ParsePid,Timeout}) ->
%%     case Timeout of
%% 	1 ->
%% 	    io:format("~p closed, reason: timeout~n",[AcceptSocket]),
%% 	    commands:quit(<<"client timeout">>, Host, AcceptSocket),
%% 	    {stop,normal,ok,whatever};
%% 	_ ->
%% 	    commands:ping(Host, AcceptSocket),
%% 	    Pid = self(),
%% 	    Receiver = gen_tcp:recv(AcceptSocket,0,1000),
%% 	    gen_server:cast(Pid,Receiver),
%% 	    {ok,{AcceptSocket,Host,ParsePid,0}}
%% 	end;
%% handle_cast({error,Reason},{Socket,Host,_ParsePid,_Timeout}) ->
%%     io:format("~p closed, reason: ~p~n", [Socket, Reason]),
%%     commands:quit(<<"crash">>, Host, Socket),
%%     {stop,Reason}.

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

%% @author Sam, Mattias, Ludwing, Per och Tomas
%% @doc Server module, handles the TCP connections.
-module(server).
-compile(export_all).
-define(TIMEOUT,60000).

%% @doc Starts the server and begin to listen for new TCP connections on port 6667.
%%      Spawns the acceptor function when a new connection connects.
start() ->
    Pid = spawn_link(fun() ->
			     {ok, Listen} = gen_tcp:listen(6667, [binary,{packet, 0}, {active, false}, {reuseaddr, true}]),
			     spawn(fun() -> acceptor(Listen) end),
			     timer:sleep(infinity)
		     end),
    {ok, Pid}.

%% @doc Accepts the socket and spawns a new listener.
%%      Spawns User, Commands and Parser actors and continues to do_recv/4 afterwards.
acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    io:format("~p connected~n", [Socket]),
    
    {ok,List} = inet:getif(),
    {ServerIP,_,_} = lists:nth(1, List),
    {ok,{hostent,ServerHostent,_,_,_,_}} = inet:gethostbyaddr(ServerIP),
    Host = {list_to_binary(inet:ntoa(ServerIP)),list_to_binary(ServerHostent)},
    
    %%ERLANG VERSION 17
    %%{ok,List} = inet:getif(),
    %%{ServerIP,_,_} = lists:nth(2, List),
    %%{ok,{hostent,ServerHostent,_,_,_,_}} = inet:gethostbyaddr(ServerIP),
    %%Host = {list_to_binary(inet:ntoa(ServerIP)),list_to_binary(ServerHostent)},

    UserPid = spawn_link(fun()-> commands:loop_user(Host,Socket) end),
    OtherPid = spawn_link(fun()-> commands:loop_other(Host,Socket, UserPid) end),
    ParserPid = spawn_link(fun()-> parser:loop(UserPid,OtherPid) end),
    
    do_recv(Socket, 0, ParserPid, Host).

%% @doc Receives all TCP message and forwards them to send_messages/2.
%%      Handles TCP connection errors and closes the connection when it should.
do_recv(Socket, Timeout, ParserPid, Host) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Message} ->
            io:format("~p: ~p~n", [Socket, Message]),
	    CommandList = binary:split(Message,<<"\n">>, [trim,global]),
	    send_messages(ParserPid, CommandList),
	    do_recv(Socket, 0, ParserPid, Host);
        {error, timeout} ->
            case Timeout of
                1 ->
		    io:format("~p closed, reason: timeout~n",[Socket]),
		    commands:quit(<<"client timeout">>, Host, Socket);
                _ ->
                    commands:ping(Host, Socket),
                    do_recv(Socket, 1, ParserPid, Host)
            end;
        {error, Reason} ->
            io:format("~p closed, reason: ~p~n", [Socket, Reason]),
            commands:quit(<<"crash">>, Host, Socket),
            exit(Reason)
    end.

%% @doc Sends the incomming message to the Parser actor.
send_messages(_,[])->
    [];
send_messages(ParserPid,[H|T]) ->
    ParserPid ! {ok, H},
    send_messages(ParserPid,T).

-module(server).
-compile(export_all).
-define(TIMEOUT,60000).

start_server() ->
    Pid = spawn_link(fun() ->
			     {ok, Listen} = gen_tcp:listen(6667, [binary,{packet, 0}, {active, false}, {reuseaddr, true}]),
			     spawn(fun() -> acceptor(Listen) end),
			     timer:sleep(infinity)
		     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    io:format("~p connected~n", [Socket]),
    UserPid = spawn_link(fun()-> commands:loop_user(Socket) end),
    OtherPid = spawn_link(fun()-> commands:loop_other(Socket, UserPid) end),
    ParserPid = spawn_link(fun()-> parser:loop(UserPid,OtherPid) end),
    do_recv(Socket, 0, ParserPid).

do_recv(Socket, Timeout, ParserPid) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Message} ->
            io:format("~p: ~p~n", [Socket, Message]),
	    CommandList = binary:split(Message,<<"\n">>, [trim,global]),
	    send_messages(ParserPid, CommandList),
	    do_recv(Socket, 0, ParserPid);
        {error, timeout} ->
            case Timeout of
                1 ->
		    io:format("~p closed, reason: timeout~n",[Socket]),
		    commands:quit(<<"">>, <<"localhost">>, Socket);
                _ ->
                    commands:ping(<<"localhost">>, Socket),
                    do_recv(Socket, 1, ParserPid)
            end;
        {error, Reason} ->
            io:format("~p closed, reason: ~p~n", [Socket, Reason]),
            commands:quit(list_to_binary(Reason), <<"localhost">>, Socket),
            exit(Reason)
    end.

send_messages(_,[])->
    [];
send_messages(ParserPid,[H|T]) ->
    ParserPid ! {ok, H},
    send_messages(ParserPid,T).

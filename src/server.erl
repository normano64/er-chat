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
    UserPid = spawn_link(fun()-> commands:loop_user() end),
    OtherPid = spawn_link(fun()-> commands:loop_other() end),
    ParserPid = spawn_link(fun()-> parser:loop(UserPid,OtherPid) end),
    do_recv(Socket, 0, ParserPid).

do_recv(Socket, Timeout, ParserPid) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Message} ->
            io:format("~p: ~p~n", [Socket, Message]),
            %%Commands = binary:split(trim(Message),<<13>>, [trim, global]),
	    %%commands(Socket, Commands),
	    ParserPid ! {ok, Message},
            do_recv(Socket, 0, ParserPid);
        {error, timeout} ->
            case Timeout of
                1 ->
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

%%is_whitespace($\n)->
 %%   true;
%%is_whitespace(<<32>>)->
  %%   true;
%% is_whitespace(_) ->
%%     false.

%% trim(Text) when is_binary(Text) ->
%%     << << X >> || <<X>> <= Text, not is_whitespace(X) >>.

%% commands(_Socket, []) ->
%%     [];
%% commands(Socket, [CommandMsg|Tail]) ->
%%     [Command, Message] = binary:split(CommandMsg,<<32>>),
%%     %%{{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
%%     %%io:format("~2..0B:~2..0B:~2..0B ~p > ~s ~s~n",[Hour, Min, Sec, Socket, Command, Message]),
%%     %%io:format("~2..0B:~2..0B:~2..0B ~p ",[Hour, Min, Sec, Socket]),
%%     command(Socket, Command, Message),
%%     commands(Socket, Tail).

%% command(Socket, <<"NICK">>, Message) ->
%%     spawn_link(fun()-> commands:nick(Message, <<"localhost">>, Socket) end);
%% command(Socket, <<"USER">>, _Message) ->
%%     UserPid = spawn_link(fun()-> commands:user(<<"Per">>,<<"Per Bergqwist">>,<<"localhost">>, Socket) end),
%%     register(userwork, UserPid);
%% command(Socket, <<"PING">>, Message) ->
%%     commands:ping(Message, <<"localhost">>, Socket);
%% command(Socket, <<"QUIT">>, Message) ->
%%     commands:quit(Message, <<"localhost">>, Socket);
%% command(_Socket, _, _Message) ->
%%     [].

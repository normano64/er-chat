-module(tcp).
-compile(export_all).

start_server(Port) ->
    Pid = spawn_link(fun() ->
			     {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
			     spawn(fun() -> acceptor(Listen) end),
			     timer:sleep(infinity)
		     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
    io:format("~2..0B:~2..0B:~2..0B ~p connected~n", [Hour, Min, Sec, Socket]),
    handle(Socket).

is_whitespace($\n)->
    true;
is_whitespace(<<32>>)->
    true;
is_whitespace(_) ->
    false.

trim(Text) when is_binary(Text) ->
    << << X >> || <<X>> <= Text, not is_whitespace(X) >>.

commands(_Socket, []) ->
    [];
commands(Socket, [CommandMsg|Tail]) ->
    [Command, Message] = binary:split(CommandMsg,<<32>>),
    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
    io:format("~2..0B:~2..0B:~2..0B ~p > ~s : ~s~n",[Hour, Min, Sec, Socket, Command, Message]),
    command(Socket, Command, Message),
    commands(Socket, Tail).

command(Socket, <<78,73,67,75>>, Message) -> % NICK
    gen_tcp:send(Socket, list_to_binary(":pebe7501 NICK pebe7501\n")),
    io:format("~p < :pebe7501 NICK pebe7501~n", [Socket]);
command(Socket, <<85,83,69,82>>, Message) -> % USER
    gen_tcp:send(Socket, [<<48, 48, 49, 32, 58>>, list_to_binary("pebe7501\n")]),
    gen_tcp:send(Socket, list_to_binary(":pebe7501 Welcome to the Internet Relay Network pebe7501\n"));
command(Socket, <<77,79,68,69>>, Message) -> % MODE
    gen_tcp:send(Socket, list_to_binary(":pebe7501 MODE pebe7501 +i\n"));
command(Socket, <<87,72,79,73,83>>, Message) -> % WHOIS
    gen_tcp:send(Socket, list_to_binary(":pebe7501 WHOIS pebe7501 :Per Bergqwist\n"));
command(Socket, <<80,73,78,71>>, Message) -> % PING send PONG
    gen_tcp:send(Socket, list_to_binary(":pebe7501 PONG localhost localhost\n")),
    gen_tcp:send(Socket, list_to_binary(":bitflip PRIVMSG pebe7501 :Hoppas det smakar massor med ost\n"));
command(Socket, <<>>, Message) -> % QUIT
    [];
command(_, _, _) ->
    [].

handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
	    Commands = binary:split(trim(Msg),<<13>>, [trim, global]),
	    commands(Socket, Commands),
            handle(Socket);
	{tcp_closed, Socket}->
	    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
            io:format("~2..0B:~2..0B:~2..0B ~p disconnected~n", [Hour, Min, Sec, Socket]),
            gen_tcp:close(Socket);
	{tcp_error, Socket, Reason} ->
            io:format("Error on Client ~p reason: ~p~n", [Socket, Reason])
    end.

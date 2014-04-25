%% @author Per Bergqwist <per.bergqwist@gmail.com>, ...

%% @doc A IRC Server made in erlang 

-module(server).
-compile(export_all).

start_server() ->
    Pid = spawn_link(fun() ->
			     {ok, Listen} = gen_tcp:listen(6667, [binary, {active, false}]),
			     spawn(fun() -> acceptor(Listen) end),
			     timer:sleep(infinity)
		     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
    io:format("~2..0B:~2..0B:~2..0B ~p connected~n", [Hour, Min, Sec, Socket]),
    handle(Socket, <<0>>).

is_whitespace($\n)->
    true;
is_whitespace(<<32>>)->
    true;
is_whitespace(_) ->
    false.

trim(Text) when is_binary(Text) ->
    << << X >> || <<X>> <= Text, not is_whitespace(X) >>.

commands(_Socket, [], User) ->
    User;
commands(Socket, [CommandMsg|Tail], User) ->
    [Command, Message] = binary:split(CommandMsg,<<32>>),
    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
    io:format("~2..0B:~2..0B:~2..0B ~p > ~s ~s~n",[Hour, Min, Sec, Socket, Command, Message]),
    io:format("~2..0B:~2..0B:~2..0B ~p ",[Hour, Min, Sec, Socket]),
    ReturnUser = command(Socket, Command, Message, User),
    commands(Socket, Tail, ReturnUser).

command(Socket, <<78,73,67,75>>, NewNick, User) -> % NICK
    gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("NICK"), <<32>>, NewNick, <<10>>]),
    io:format("< NICK ~s~n", [NewNick]),
    NewNick;
command(Socket, <<85,83,69,82>>, _Message, User) -> % USER
    gen_tcp:send(Socket, [<<48, 48, 49, 32, 58>>, User, <<10>>]),
    gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("Welcome to the Internet Relay Network"), <<32>>, User, <<10>>]),
    io:format("< Welcome to the Internet Relay Network ~s~n", [User]),
    User;
command(Socket, <<77,79,68,69>>, _Message, User) -> % MODE
    gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("MODE"), <<32>>, User, <<32>>, list_to_binary("+i\n")]),
    io:format("< MODE pebe7501 +i~n"),
    User;
command(Socket, <<87,72,79,73,83>>, _Message, User) -> % WHOIS
    gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("WHOIS pebe7501 :Per Bergqwist\n")]),
    io:format("< WHOIS pebe7501 :Per Bergqwist~n"),
    User;
command(Socket, <<80,73,78,71>>, _Message, User) -> % PING send PONG
    gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("PONG localhost localhost\n")]),
    gen_tcp:send(Socket, [<<58>>, list_to_binary("bitflip"), <<32>>, list_to_binary("PRIVMSG"), <<32>>, User, <<32>>, <<58>>, list_to_binary("Hoppas det smakar massor med ost\n")]),
    io:format("< PONG localhost localhost~n"),
    User;
command(Socket,<<74,79,73,78>>, Message, User)-> %JOIN message = #detduskrivit
    gen_tcp:send(Socket,[<<51,51,49,32>>,Message,<<32,58>>,list_to_binary("No Topic Is Set\n")]),
    gen_tcp:send(Socket,[<<58>>,User, <<74,79,73,78>>, Message,<<10>>]),
    io:format("Who is knoking on my door???~n");
command(_Socket, <<>>, _Message, User) -> % QUIT
    User;
command(_, _, _, User) ->
    User.

handle(Socket, User) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            io:format(": ~p~n", [Msg]),
	    Commands = binary:split(trim(Msg),<<13>>, [trim, global]),
	    ReturnUser = commands(Socket, Commands, User),
            handle(Socket, ReturnUser);
	{tcp_closed, Socket}->
	    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
            io:format("~2..0B:~2..0B:~2..0B ~p disconnected~n", [Hour, Min, Sec, Socket]),
            gen_tcp:close(Socket);
	{tcp_error, Socket, Reason} ->
            io:format("Error on Client ~p reason: ~p~n", [Socket, Reason])
    end.

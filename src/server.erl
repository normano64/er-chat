%% @author Per Bergqwist <per.bergqwist@gmail.com>, ...

%% @doc A IRC Server made in erlang 

-module(server).
-compile(export_all).

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
    %%{{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
    %%io:format("~2..0B:~2..0B:~2..0B ~p > ~s ~s~n",[Hour, Min, Sec, Socket, Command, Message]),
    %%io:format("~2..0B:~2..0B:~2..0B ~p ",[Hour, Min, Sec, Socket]),
    command(Socket, Command, Message),
    commands(Socket, Tail).

command(Socket, <<"NICK">>, Message) ->
    ParentPid = self(),
    spawn_link(fun()-> commands:nick(Message, ParentPid, <<"localhost">>, Socket) end);
command(Socket, <<"USER">>, _Message) ->
    commands:user(<<"localhost">>, Socket);
command(Socket, <<"PING">>, Message) ->
    commands:ping(Message, <<"localhost">>, Socket);
command(_Socket, _, _Message) ->
    [].

%% command(Socket, <<"NICK">>, NewNick, User, Hostname) ->
%%     %gen_tcp:send(Socket, [<<":">>, User, <<" NICK ">>, NewNick, <<"\r\n">>]),
%%     io:format("< NICK ~s~n", [NewNick]),
%%     NewNick;
%% command(Socket, <<"USER">>, _Message, User, Hostname) ->
%%     gen_tcp:send(Socket, [<<":localhost 001 ">>, User, <<" :Welcome to the Internet Relay Chat Network ">>, User, <<"\r\n">>]),
%%     gen_tcp:send(Socket, [<<":localhost 002 ">>, User, <<" :Your host is localhost[130.238.93.82], running version er-chat-alpha-01\r\n">>]),
%%     io:format("< Welcome to the Internet Relay Network ~s@~w~n", [User, Hostname]),
%%     User;
%% command(Socket, <<"MODE">>, _Message, User, Hostname) -> % MODE
%%     gen_tcp:send(Socket, [<<":">>, User, <<" MODE ">>, User, <<" :+i\r\n">>]),
%%     io:format("< MODE pebe7501 +i~n"),
%%     User;
%% command(Socket, <<"WHOIS">>, _Message, User, Hostname) -> % WHOIS
%%     gen_tcp:send(Socket, [<<58>>, User, <<32>>, list_to_binary("WHOIS pebe7501 :Per Bergqwist\n")]),
%%     io:format("< WHOIS pebe7501 :Per Bergqwist~n"),
%%     User;
%% command(Socket, <<"PING">>, _Message, User, Hostname) -> % PING send PONG
%%     gen_tcp:send(Socket, [<<":localhost PONG localhost :localhost\r\n">>]),
%%     %gen_tcp:send(Socket, [<<58>>, list_to_binary("bitflip"), <<32>>, list_to_binary("PRIVMSG"), <<32>>, User, <<32>>, <<58>>, list_to_binary("Hoppas det smakar massor med ost\r\n")]),
%%     io:format("< PONG localhost localhost~n"),
%%     User;
%% command(Socket,<<"JOIN">>, Message, User, Hostname)-> %JOIN message = #detduskrivit
%%     gen_tcp:send(Socket, [<<":">>, User, <<"!per@localhost JOIN :">>, Message, <<"\r\n">>]),
%%     gen_tcp:send(Socket, [<<":localhost 331 ">>, User, <<" ">>, Message, <<" :No Topic Is Set\r\n">>]),
%%     gen_tcp:send(Socket, [<<":localhost 353 ">>,User, <<" @ ">>, Message,<<" :Ostmackan @OP @enHest Yaourt\r\n">>]),
%%     io:format("Who is knoking on my door???~n");
%% command(_Socket, <<>>, _Message, User, Hostname) -> % QUIT
%%     User;
%% command(_, _, _, User, Hostname) ->
%%     User.

handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            io:format("~p: ~p~n", [Socket, Msg]),
	    Commands = binary:split(trim(Msg),<<13>>, [trim, global]),
	    %%ReturnUser =
            commands(Socket, Commands),
            handle(Socket);
	{tcp_closed, Socket}->
	    {{_,_,_},{Hour,Min,Sec}} = erlang:localtime(),
            io:format("~2..0B:~2..0B:~2..0B ~p disconnected~n", [Hour, Min, Sec, Socket]),
            database:delete_socket(Socket),
            gen_tcp:close(Socket);
	{tcp_error, Socket, Reason} ->
            io:format("Error on Client ~p reason: ~p~n", [Socket, Reason])
    end.

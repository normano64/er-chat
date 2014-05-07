-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

loop_user(Host,Socket)->
    receive
	{user,[User,_Steps,_Star,RealName]} ->
	    user(User,RealName,Host,Socket),
	    loop_user(Host,Socket);
        {nick_ok}->
            receive
                {user,[User,_Steps,_Star,RealName]} ->
                    user(User,RealName,Host,Socket,nick_ok)
            end,
            loop_user(Host,Socket);
        Error ->
            io:format("Error user:~p~n",[Error])
    end.

loop_other(Host,Socket, UserPid)->
    receive 
	{nick,[Nick]} ->
	    nick(Nick, UserPid, Host, Socket),
	    loop_other(Host, Socket, UserPid);
        {ping,[Server]} ->
	    pong(Host, Server, Socket),
            loop_other(Host,Socket, UserPid);
        {quit,[Message]} ->
	    quit(Message, Host, Socket),
            loop_other(Host,Socket, UserPid);
	{join, [Channel]} ->
	    join(Channel,Host,Socket),
	    loop_other(Host, Socket, UserPid);
        {unknown, Command}->
            {_ServerIP,ServerHostent} = Host,
            gen_tcp:send(Socket, ?REPLY_UNKNOWNCOMMAND),
            loop_other(Host,Socket, UserPid);
        Error ->
            io:format("Error nick:~p~n",[Error])
    end.

user(User, RealName, {ServerIP,ServerHostent}, Socket)->
    case database:check_socket(Socket) of
        {_,UserTuple} when is_list(UserTuple) orelse element(2,UserTuple) == empty ->
            receive
                {nick_ok} ->
                    {_,Nick} = database:get_nick(Socket),
                    if
                        Nick =/= [] ->
                            {_,Port} = inet:port(Socket),
                            database:update_user(Socket, User, RealName),
                            gen_tcp:send(Socket, ?REPLY_WELCOME), 
                            gen_tcp:send(Socket, ?REPLY_YOURHOST);
                        true ->
                            nick_not_registered
                    end
            end;
        _ ->
            {_,Nick} = database:get_nick(Socket),
            gen_tcp:send(Socket, ?REPLY_ALREADYREGISTERD)  
    end.
user(User, RealName, {ServerIP,ServerHostent}, Socket, nick_ok)->
    case database:check_socket(Socket) of
        {_,UserTuple} when is_list(UserTuple) orelse element(2,UserTuple) == empty ->
            {_,Nick} = database:get_nick(Socket),
            if
                Nick =/= [] ->
                    {_,Port} = inet:port(Socket),
                    database:update_user(Socket, User, RealName),
                    gen_tcp:send(Socket, ?REPLY_WELCOME), 
                    gen_tcp:send(Socket, ?REPLY_YOURHOST);
                true ->
                    nick_not_registered
            end;
        _ ->
            {_,Nick} = database:get_nick(Socket),
            gen_tcp:send(Socket, ?REPLY_ALREADYREGISTERD)
    end.

nick(Nick, UserPid, {_ServerIP,ServerHostent}, Socket)->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    {_,{IP,_}} = inet:sockname(Socket),
                    {_,{_,Hostent,_,_,_,_}} = inet:gethostbyaddr(IP),
                    database:insert_user(Socket, empty, Nick, ServerHostent, list_to_binary(Hostent), empty),
		    UserPid ! {nick_ok};
		{_,[{user,_, User, OldNick, _, Hostent, _}]} ->
                    database:update_nick(Socket, Nick),
		    gen_tcp:send(Socket, ?REPLY_UPDATENICK) 
            end;
        _  ->
            gen_tcp:send(Socket, ?REPLY_NICKNAMEINUSE)
    end.
            
ping({_ServerIP,ServerHostent}, Socket)->
    gen_tcp:send(Socket, ?REPLY_PING).

pong({_ServerIP,ServerHostent}, _Server, Socket)->
    {_, Nick} = database:get_nick(Socket),
    gen_tcp:send(Socket, ?REPLY_PONG).  


quit(_Message, {_ServerIP,_ServerHostent}, Socket)->
    case database:check_socket(Socket) of
        {_,[{user,_, User, Nick, _, Hostent, _}]} ->
            gen_tcp:send(Socket, ?REPLY_QUIT), 
            database:delete_socket(Socket);
        {_,[]} ->
            already_closed
    end.


%% mode(, Socket)->
%%  
convert_nicklist([{Status,Nick}|T])->   
    convert_nicklist(T,[Status,Nick]).
convert_nicklist([], Ack)->
    list_to_binary(Ack);
convert_nicklist([{Status, Nick}|T], Ack)->
    convert_nicklist(T,[Status, Nick,<<" ">>] ++ Ack).

join(Channel, {_ServerIP,ServerHostent}, Socket)->
    {_,[{user,_,User,Nick,_,Hostent,_}]} = database:check_socket(Socket),
    UserList = [<<"@">>,Nick],
    case database:check_channel(Channel) of
	{_,[channel,_,Users]} ->
	    database:join_channel(Channel,Nick),
	    UserList = convert_nicklist(Users),
	    gen_tcp:send(Socket,?REPLY_JOINCHANNEL),
	    gen_tcp:send(Socket,?REPLY_JOINTOPIC),
	    gen_tcp:send(Socket,?REPLY_JOINNAMREPLY);
	_ ->
	    database:insert_channel(Channel,{<<"@">>,Nick},<<"">>),
	    gen_tcp:send(Socket,?REPLY_JOINCHANNEL),
	    gen_tcp:send(Socket,?REPLY_JOINNAMREPLY)
    end.

%% privmsg(, Socket)->
%%     .

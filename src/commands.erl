-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

loop_user(Socket)->
    receive 
	{user, [User,_Steps,_Star,RealName]} ->
	    user(User,RealName,<<"localhost">>,Socket),
	    loop_user(Socket)
	end.

loop_other(Socket, UserPid)->
    receive 
	{nick,Nick} ->
	    nick(Nick, <<"localhost">>, Socket,UserPid),
	    loop_user(Socket)
	end.

user(User, RealName, Server, Socket)->
    case database:check_socket(Socket) of
        {_,UserTuple} when is_list(UserTuple) orelse element(2,UserTuple) == empty ->
            receive
                {nick_ok} ->
                    {_,Nick} = database:get_nick(Socket),
                    if
                        Nick =/= [] ->
                            {_,Hostname} = inet:gethostname(),
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

nick(Nick, Server, Socket, UserPid)->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    {_,{IP,_}} = inet:sockname(Socket),
                    {_,{_,Hostent,_,_,_,_}} = inet_res:gethostbyaddr(IP),
                    database:insert_user(Socket, empty, Nick, Server, list_to_binary(Hostent), empty),
		    UserPid ! {nick_ok};
		{_,[{user,_, User, OldNick, _, Hostent, _}]} ->
                    database:update_nick(Socket, Nick),
		    gen_tcp:send(Socket, ?REPLY_UPDATENICK) 
            end;
        _  ->
            gen_tcp:send(Socket, ?REPLY_NICKNAMEINUSE)
    end.
            
ping(Server, Socket)->
    gen_tcp:send(Socket, ?REPLY_PING).

quit(_Message, _Server, Socket)->
    case database:check_socket(Socket) of
        {_,[{user,_, User, Nick, _, Hostent, _}]} ->
            gen_tcp:send(Socket, ?REPLY_QUIT), 
            database:delete_socket(Socket);
        {_,[]} ->
            already_closed
    end.


%% mode(, Socket)->
%%     .

%% join(, Socket)->
%%     .

%% privmsg(, Socket)->
%%     .

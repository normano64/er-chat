-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

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
                            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_WELCOME, <<" ">>, Nick, <<" :Welcome to the Internet Relay Network ">>, Nick, <<"\r\n">>]),
                            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_YOURHOST, <<" ">>, Nick, <<" :Your host is localhost[">>, list_to_binary(Hostname), <<"/">>, list_to_binary(integer_to_list(Port)), <<"], running version er-chat-alpha-01\r\n">>]);
                        true ->
                            nick_not_registered
                    end
            end;
        _ ->
            {_,Nick} = database:get_nick(Socket),
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?ERR_ALREADYREGISTRED, <<" ">>, Nick, <<" :You may not reregister\r\n">>])
    end.

nick(Nick, Server, Socket)->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    {_,{IP,_}} = inet:sockname(Socket),
                    {_,{_,Hostent,_,_,_,_}} = inet_res:gethostbyaddr(IP),
                    database:insert_user(Socket, empty, Nick, Server, list_to_binary(Hostent), empty),
                    UserPid = whereis(userwork),
                    UserPid ! {nick_ok};
                {_,[{user,_, User, OldNick, _, Hostent, _}]} ->
                    database:update_nick(Socket, Nick),
                    
                    gen_tcp:send(Socket, [<<":">>, OldNick, <<"!">>, User, <<"@">>, Hostent, <<" NICK :">>, Nick, <<"\r\n">>])
            end;
        _  ->
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?ERR_NICKNAMEINUSE, <<" * ">>, Nick, <<" :Nickname is already in use.\r\n">>])
    end.
            
ping(Server, Socket)->
    gen_tcp:send(Socket, [<<"PING :">>, Server, <<"\r\n">>]).

%% mode(, Socket)->
%%     .

%% join(, Socket)->
%%     .

%% privmsg(, Socket)->
%%     .

quit(Message, Server, Socket)->
    case database:check_socket(Socket) of
        {_,[{user,_, User, Nick, _, Hostent, _}]} ->
            gen_tcp:send(Socket, [<<":">>, Nick, <<"!">>, User, <<"@">>, Hostent, <<" QUIT :">>, "Gone to buy cheese.", <<"\r\n">>]),
            database:delete_socket(Socket);
        {_,[]} ->
            already_closed
    end.

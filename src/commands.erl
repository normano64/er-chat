-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

loop_user(Host,Socket) ->
    receive
        {user,[User,_Steps,_Star,RealName]} ->
            user(User,RealName,Host,Socket),
            loop_user(Host,Socket);
        {nick_ok} ->
            receive
                {user,[User,_Steps,_Star,RealName]} ->
                    user(User,RealName,Host,Socket,nick_ok)
            end,
            loop_user(Host,Socket);
        Error ->
            io:format("Error user:~p~n",[Error])
    end.

loop_other(Host,Socket,UserPid) ->
    receive
        {nick,[Nick]} ->
            nick(Nick,UserPid,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {ping,[Server]} ->
            pong(Host,Server,Socket),
            loop_other(Host,Socket,UserPid);
        {pong,_} ->
            loop_other(Host,Socket,UserPid);
        {quit,[Message]} ->
            quit(Message,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {join,[Channel]} ->
            join(Channel,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {privmsg,[Target,Message]} ->
            privmsg(Target,Message,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {part,[Target,Message]} ->
            part(Target,Message,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {part,[Target]} ->
            part(Target,<<"">>,Host,Socket),
            loop_other(Host,Socket,UserPid);
        {unknown,Command} ->
            {_ServerIP,ServerHostent} = Host,
            gen_tcp:send(Socket,?REPLY_UNKNOWNCOMMAND),
            loop_other(Host,Socket,UserPid);
        Error ->
            io:format("Error nick:~p~n",[Error])
    end.

user(User,RealName,{ServerIP,ServerHostent},Socket) ->
    case database:check_socket(Socket) of
        {_,UserTuple} when is_list(UserTuple) orelse element(2,UserTuple) == empty ->
            receive
                {nick_ok} ->
                    {_,Nick} = database:get_nick(Socket),
                    if
                        Nick =/= [] ->
                            {_,Port} = inet:port(Socket),
                            database:update_user(Socket,User,RealName),
                            gen_tcp:send(Socket,?REPLY_WELCOME),
                            gen_tcp:send(Socket,?REPLY_YOURHOST);
                        true ->
                            nick_not_registered
                    end
            end;
        _ ->
            {_,Nick} = database:get_nick(Socket),
            gen_tcp:send(Socket,?REPLY_ALREADYREGISTERD)
    end.
user(User,RealName,{ServerIP,ServerHostent},Socket,nick_ok) ->
    case database:check_socket(Socket) of
        {_,UserTuple} when is_list(UserTuple) orelse element(2,UserTuple) == empty ->
            {_,Nick} = database:get_nick(Socket),
            if
                Nick =/= [] ->
                    {_,Port} = inet:port(Socket),
                    database:update_user(Socket,User,RealName),
                    gen_tcp:send(Socket,?REPLY_WELCOME),
                    gen_tcp:send(Socket,?REPLY_YOURHOST);
                true ->
                    nick_not_registered
            end;
        _ ->
            {_,Nick} = database:get_nick(Socket),
            gen_tcp:send(Socket,?REPLY_ALREADYREGISTERD)
    end.

nick(Nick,UserPid,{_ServerIP,ServerHostent},Socket) ->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    {_,{IP,_}} = inet:sockname(Socket),
                    {_,{_,Hostent,_,_,_,_}} = inet:gethostbyaddr(IP),
                    database:insert_user(Socket,empty,Nick,ServerHostent,list_to_binary(Hostent),empty),
                    UserPid ! {nick_ok};
                {_,[{user,_,User,OldNick,_,Hostent,_,_}]} ->
                    database:update_nick(Socket,Nick),
                    gen_tcp:send(Socket,?REPLY_UPDATENICK)
                    %%send_new_nick(OldNick,Nick,User,Hostent,)
            end;
        _  ->
            gen_tcp:send(Socket,?REPLY_NICKNAMEINUSE)
    end.

ping({_ServerIP,ServerHostent},Socket) ->
    gen_tcp:send(Socket,?REPLY_PING).

pong({_ServerIP,ServerHostent},_Server,Socket) ->
    {_,Nick} = database:get_nick(Socket),
    gen_tcp:send(Socket,?REPLY_PONG).


quit(_Message,{_ServerIP,_ServerHostent},Socket) ->
    case database:check_socket(Socket) of
        {_,[{user,_,User,Nick,_,Hostent,_,_}]} ->
            gen_tcp:send(Socket,?REPLY_QUIT),
            database:delete_socket(Socket);
        {_,[]} ->
            already_closed
    end.

convert_nicklist([]) ->
    <<"">>;
convert_nicklist([{Status,Nick}|T]) ->
    convert_nicklist(T,[Status,Nick]).
convert_nicklist([],Ack) ->
    list_to_binary(Ack);
convert_nicklist([{Status,Nick}|T],Ack) ->
    convert_nicklist(T,[Status,Nick,<<" ">>] ++ Ack).

send_join_replies([],_Socket,_Channel) ->
    ok;
send_join_replies([{_Status,NickDb}|T],Socket,Channel) ->
    {_,[{user,_,User,Nick,_,Hostent,_,_}]} = database:check_socket(Socket),
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_JOINCHANNEL),
    send_join_replies(T,Socket,Channel).

join(Channel,{_ServerIP,ServerHostent},Socket) ->
    {_,[{user,_,User,Nick,_,Hostent,_,ChannelList}]} = database:check_socket(Socket),
    case database:check_channel(Channel) of
        {_,[{channel,_,Users,Topic}]} ->
            case lists:member(Channel,ChannelList) of
                false ->
                    database:join_channel(Channel,{<<"">>,Nick},Socket),
                    UserList = convert_nicklist(Users),
                    send_join_replies(Users,Socket,Channel),
                    gen_tcp:send(Socket,?REPLY_JOINCHANNEL),
                    if
                        Topic == <<"">> ->
                            gen_tcp:send(Socket,?REPLY_JOINNOTOPIC);
                        true ->
                            gen_tcp:send(Socket,?REPLY_JOINTOPIC)
                    end,
                    gen_tcp:send(Socket,?REPLY_JOINNAMREPLY),
                    gen_tcp:send(Socket,?REPLY_ENDOFNAMES);
                _ ->
                    []
            end;
        _ ->
            database:insert_channel(Channel,{<<"@">>,Nick},<<"">>),
            UserList = [<<"@">>,Nick],
            gen_tcp:send(Socket,?REPLY_JOINCHANNEL),
            gen_tcp:send(Socket,?REPLY_JOINNAMREPLY),
            gen_tcp:send(Socket,?REPLY_ENDOFNAMES)
    end.

send_privmsg([],_Target,_Message,_Socket) ->
    ok;
send_privmsg([{_Status,NickDb}|T],Target,Message,Socket) ->
    {_,[{user,_,User,Nick,_,Hostent,_,_}]} = database:check_socket(Socket),
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    if
        NickDb =/= Nick ->
            gen_tcp:send(SocketToSendTo,?REPLY_PRIVMSG);
        true ->
            []
    end,
    send_privmsg(T,Target,Message,Socket).

privmsg(Target,Message,{_ServerIP,_ServerHostent},Socket) ->
    case database:check_channel(Target) of
        {_,[{channel,_,Users,_Topic}]} ->
            send_privmsg(Users,Target,Message,Socket);
        _ ->
            []
    end.

send_part([],_Target,_Message,_Socket) ->
    ok;
send_part([{_Status,NickDb}|T],Target,Message,Socket) ->
    {_,[{user,_,User,Nick,_,Hostent,_,_}]} = database:check_socket(Socket),
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_PART),
    send_part(T,Target,Message,Socket).

part(Target,Message,{_ServerIP,_ServerHostent},Socket) ->
    {_,[{user,_,User,Nick,_,Hostent,_,ChannelList}]} = database:check_socket(Socket),
    case database:check_channel(Target) of
        {_,[{channel,_,Users,Topic}]} ->
            case lists:member(Target,ChannelList) of
                true ->
                    send_part(Users,Target,Message,Socket),
                    database:part_channel(Target,Nick,Socket);
                false ->
                    []
            end
    end.

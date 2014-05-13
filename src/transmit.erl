-module(transmit).
-compile(export_all).
-include("rpl_macro.hrl").

channel_change_nick([],_NewNick,UserList,_Socket) ->
    UserList;
channel_change_nick([H|T],NewNick,UserList,Socket) ->
    {_,[{channel,_Channel,NickList,_Topic}]} = database:check_channel(H),
    database:change_channel_nick(H,NewNick,Socket),
    channel_change_nick(T,NewNick,lists:umerge(UserList,NickList),Socket).

send_new_nick([],_OldNick,_NewNick,_User,_Hostent) ->
    ok;
send_new_nick([{_,SendToNick}|T],OldNick,Nick,User,Hostent) ->
    {_,[{user,Socket,_,_,_Server,_,_RealName,_ChannelList}]} = database:check_nick(SendToNick),
    gen_tcp:send(Socket,?REPLY_UPDATENICK),
    send_new_nick(T,OldNick,Nick,User,Hostent).

convert_nicklist([]) ->
    <<"">>;
convert_nicklist([{Status,Nick}|T]) ->
    convert_nicklist(T,[Status,Nick]).
convert_nicklist([],Ack) ->
    list_to_binary(Ack);
convert_nicklist([{Status,Nick}|T],Ack) ->
    convert_nicklist(T,[Status,Nick,<<" ">>] ++ Ack).

send_join_replies([],_Channel,_Nick,_User,_Hostent) ->
    ok;
send_join_replies([{_Status,NickDb}|T],Channel,Nick,User,Hostent) ->
    io:format("~p:~p:~p:~p~n",[Channel,Nick,User,Hostent]),
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_JOINCHANNEL),
    send_join_replies(T,Channel,Nick,User,Hostent).

send_privmsg([],_Target,_Message,_Nick,_User,_Hostent) ->
    ok;
send_privmsg([{_Status,NickDb}|T],Target,Message,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    if
        NickDb =/= Nick ->
            gen_tcp:send(SocketToSendTo,?REPLY_PRIVMSG);
        true ->
            []
    end,
    send_privmsg(T,Target,Message,Nick,User,Hostent).

send_part([],_Target,_Message,_Nick,_User,_Hostent) ->
    ok;
send_part([{_Status,NickDb}|T],Target,Message,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_PART),
    send_part(T,Target,Message,Nick,User,Hostent).

send_new_topic([],Channel,Topic,Nick,User,Hostent)) ->
    ok;
send_new_topic([SendUser|Tail],Channel,Topic,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_NEWTOPIC),
    send_new_topic(Tail,Channel,Topic,Nick,User,Hostent).

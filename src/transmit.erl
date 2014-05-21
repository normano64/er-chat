-module(transmit).
-compile(export_all).
-include("rpl_macro.hrl").

is_channel(Channel)->
    case binary:first(Channel) of
        35 ->
            true;
        _ ->
            false
    end.

get_comment([_Nick|Comment])->
    Comment.

extract_nick([],Ack)->
    Ack;
extract_nick([{_Status,Nick}|T],Ack) ->
    extract_nick(T,[Nick|Ack]).
    
channel_change_nick([],_NewNick,UserList,_Socket) ->
    UserList;
channel_change_nick([H|T],NewNick,UserList,Socket) ->
    {_,[{channel,_Channel,NickList,_Topic}]} = database:check_channel(H),
    NewNickList = extract_nick(NickList,[]),
    database:change_channel_nick(H,NewNick,Socket),
    channel_change_nick(T,NewNick,lists:umerge(UserList,NewNickList),Socket).

send_new_nick([],_OldNick,_NewNick,_User,_Hostent) ->
    ok;
send_new_nick([SendToNick|T],OldNick,Nick,User,Hostent) ->
    io:format("DAFUUUUQ ~n"),
    {_,[{user,Socket,_,_,_Server,_,_RealName,_ChannelList}]} = database:check_nick(SendToNick),
    io:format("H*R VI FAILAR?!?! ~n"),
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

send_new_topic([],_Channel,_Topic,_Nick,_User,_Hostent) ->
    ok;
send_new_topic([{_Status,NickDb}|Tail],Channel,Topic,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_NEWTOPIC),
    send_new_topic(Tail,Channel,Topic,Nick,User,Hostent).


send_kick_comment([],_Nick,_User,_Hostent,_Target,_TargetChannel, _Comment)->
    ok;
send_kick_comment([{_Status,NickDb}|Tail],{Lnick,Nick}, User, Hostent, Target, TargetChannel,Comment) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_KICK_COMMENT), 
    send_kick_comment(Tail,{Lnick,Nick},User,Hostent,Target,TargetChannel,Comment).

send_kick([],_Nick,_User,_Hostent,_Target,_TargetChannel)->
    ok;
send_kick([{_Status,NickDb}|Tail],{Lnick,Nick},User,Hostent,Target,TargetChannel)->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_KICK_NOCOMMENT), 
    send_kick(Tail,{Lnick,Nick},User,Hostent,Target,TargetChannel).

send_wholist([],_Socket,_Nick,_ServerHostent,_Channel) ->
    ok;
send_wholist([{Status,NickDb}|Tail],Socket,Nick,ServerHostent,Channel) ->
    {_,[{user,_,User,_,UserServer,UserHostent,RealName,_ChannelList}]} = database:check_nick(NickDb),
    gen_tcp:send(Socket,?REPLY_WHO),
    send_wholist(Tail,Socket,Nick,ServerHostent,Channel).

send_list([],_Socket,_ServerHostent,_Channel,_Topic)->
    ok;
send_list([{_Status,Nick}|Tail],Socket,ServerHostent,Channel,Topic) ->
    Number = length(Tail) + 1,
    gen_tcp:send(Socket,?REPLY_LIST),
    send_list(Tail,Socket,ServerHostent,Channel,Topic).

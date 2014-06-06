-module(transmit).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("rpl_macro.hrl").
-export_type([any/0]).

%% @doc Extracts every user from a channel that need to be notified about a
%% nickname that has been changed.
channel_change_nick([],_NewNick,UserList,_Socket) ->
    UserList;
channel_change_nick([H|T],NewNick,UserList,Socket) ->
    {_,[{channel,_Channel,NickList,_Topic}]} = database:check_channel(H),
    NewNickList = extract_nick(NickList,[]),
    database:change_channel_nick(H,NewNick,Socket),
    channel_change_nick(T,NewNick,lists:umerge(UserList,NewNickList),Socket).

%% @doc Send information to every user in list users that a users has changed nickname.
send_new_nick([],_OldNick,_NewNick,_User,_Hostent) ->
    ok;
send_new_nick([SendToNick|Users],OldNick,Nick,User,Hostent) ->
    {_,[{user,Socket,_,_,_Server,_,_RealName,_ChannelList}]} = database:check_nick(SendToNick),
    gen_tcp:send(Socket,?REPLY_UPDATENICK),
    send_new_nick(Users,OldNick,Nick,User,Hostent).

%% @doc Send information to every user in list Users that a users has joined the same
%% channel that Users are in.
send_join_replies([],_Channel,_Nick,_User,_Hostent) ->
    ok;
send_join_replies([{_Status,NickDb}|Users],Channel,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_JOINCHANNEL),
    send_join_replies(Users,Channel,Nick,User,Hostent).

%% @doc Send information to every user in list Users that a private message has been sent
%% from user Target with text Message.
send_privmsg([],_Target,_Message,_Nick,_User,_Hostent) ->
    ok;
send_privmsg([{_Status,NickDb}|Users],Target,Message,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    if
        NickDb =/= Nick ->
            gen_tcp:send(SocketToSendTo,?REPLY_PRIVMSG);
        true ->
            []
    end,
    send_privmsg(Users,Target,Message,Nick,User,Hostent).

%% @doc Send information to every user in list Users that a user has parted (left) the
%% same channel that they are in. Every user in that channel need to be notified about this.
send_part([],_Target,_Message,_Nick,_User,_Hostent) ->
    ok;
send_part([{_Status,NickDb}|Users],Target,Message,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_PART),
    send_part(Users,Target,Message,Nick,User,Hostent).

%% @doc Send information to every user in list Users that a user has changed the topic in the 
%% channel that they are in. Every user in that channel need to be notified about this.
send_new_topic([],_Channel,_Topic,_Nick,_User,_Hostent) ->
    ok;
send_new_topic([{_Status,NickDb}|Users],Channel,Topic,Nick,User,Hostent) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_NEWTOPIC),
    send_new_topic(Users,Channel,Topic,Nick,User,Hostent).

%% @doc Send information to every user in list Users that a user has been kicked from a channel
%% with a comment Comment. Every user in that channel need to be notified about this.
send_kick_comment([],_Nick,_User,_Hostent,_Target,_TargetChannel, _Comment)->
    ok;
send_kick_comment([{_Status,NickDb}|Users],{Lnick,Nick}, User, Hostent, Target, TargetChannel,Comment) ->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_KICK_COMMENT), 
    send_kick_comment(Users,{Lnick,Nick},User,Hostent,Target,TargetChannel,Comment).

%% @doc Send information to every user in list Users that a user has been kicked. 
%% Every user in that channel need to be notified about this.
send_kick([],_Nick,_User,_Hostent,_Target,_TargetChannel)->
    ok;
send_kick([{_Status,NickDb}|Users],{Lnick,Nick},User,Hostent,Target,TargetChannel)->
    {_,[{user,SocketToSendTo,_,_,_,_,_,_}]} = database:check_nick(NickDb),
    gen_tcp:send(SocketToSendTo,?REPLY_KICK_NOCOMMENT), 
    send_kick(Users,{Lnick,Nick},User,Hostent,Target,TargetChannel).

%% @doc Send information to port Socket about every user in list Users. 
send_wholist([],_Socket,_Nick,_ServerHostent,_Channel) ->
    ok;
send_wholist([{Status,NickDb}|Users],Socket,Nick,ServerHostent,Channel) ->
    {_,[{user,_,User,_,UserServer,UserHostent,RealName,_ChannelList}]} = database:check_nick(NickDb),
    gen_tcp:send(Socket,?REPLY_WHO),
    send_wholist(Users,Socket,Nick,ServerHostent,Channel).

%% @doc Send information to port Socket about every channel on the server.
send_list([],_Socket,_ServerHostent,_Channel,_Topic)->
    ok;
send_list([{_Status,Nick}|Users],Socket,ServerHostent,Channel,Topic) ->
    Number = length(Users) + 1,
    gen_tcp:send(Socket,?REPLY_LIST),
    send_list(Users,Socket,ServerHostent,Channel,Topic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                                    Help functions                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
is_channel(Channel)->
    case binary:first(Channel) of
        35 ->
            true;
        _ ->
            false
    end.

%% @hidden
get_comment([_Nick|Comment])->
    Comment.

%% @hidden
extract_nick([],Ack)->
    Ack;
extract_nick([{_Status,Nick}|T],Ack) ->
    extract_nick(T,[Nick|Ack]).

%% @hidden
convert_nicklist([]) ->
    <<"">>;
convert_nicklist([{Status,Nick}|T]) ->
    convert_nicklist(T,[Status,Nick]).
%% @hidden
convert_nicklist([],Ack) ->
    list_to_binary(Ack);
convert_nicklist([{Status,Nick}|T],Ack) ->
    convert_nicklist(T,[Status,Nick,<<" ">>] ++ Ack).

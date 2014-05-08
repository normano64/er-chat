-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(channel,{id, users, topic}).
-record(user,{socket, user, nick, server,hostent, realname, channel_list}). %%add channel list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Database functions                                          %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_db()->
    ListNodes = [node()],
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{type,set}]),
    mnesia:create_table(channel,[{attributes,record_info(fields,channel)},{disc_copies,ListNodes},{type,set}]).

traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

delete_table_db(Table)->
    mnesia:delete_table(Table).

start()->
    mnesia:start().
stop()-> 
    mnesia:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Socket/nick functions                                       %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_user(Socket,User,Nick,Server,Hostent,Realname)->
    Data = #user{socket=Socket,user=User,nick=Nick, server=Server,hostent=Hostent, realname=Realname, channel_list = []},
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

check_socket(Socket)->
    F = fun() ->
		SocketDb = mnesia:read(user,Socket),
		SocketDb
	end,
    mnesia:transaction(F).

check_nick(Nick)->
    F = fun() ->
		Found = mnesia:match_object({user,'_','_',Nick,'_','_','_','_'}),
		Found
	end,
    mnesia:transaction(F).

find_nick([])->
    [];
find_nick({_,_,_,Nick,_,_,_,_})->
    Nick.

get_nick(Socket)->
    F = fun()->
		{_,List} = check_socket(Socket),
		find_nick(get_head(List))
	end,
    mnesia:transaction(F).

update_nick(Socket, Nick)->
    F = fun()->
		[P] = mnesia:wread({user,Socket}),
		mnesia:write(P#user{nick = Nick})
	end,
    mnesia:transaction(F).

update_user(Socket, User, Realname)->
    F = fun()->
		[P] = mnesia:wread({user,Socket}),
		mnesia:write(P#user{user=User,realname=Realname})
	end,
    mnesia:transaction(F).

delete_socket(Socket)->
    F = fun()->
		mnesia:delete({user,Socket})
	end,
    mnesia:transaction(F).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Channel Functions                                           %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_channellist({_,_,_,_,_,_,_,ChannelList})->
    ChannelList.

insert_channel(ChannelName, User, Topic) ->
    Data = #channel{id = ChannelName,users = [User], topic = Topic},
    F = fun()->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

update_list(#channel{id = _Id,users = UserList}, User)->
    [User | UserList].

join_channel(ChannelName, Nick, Socket)->
    F = fun()->
		[Channel]= mnesia:wread({channel,ChannelName}),
		mnesia:write(Channel#channel{id = ChannelName,users=update_list(Channel,Nick)}),
		{_,List} = check_socket(Socket),
		ChannelList = find_channellist(get_head(List)),
		[P] =  mnesia:wread({user,Socket}),
		mnesia:write(P#user{channel_list = [ChannelName | ChannelList]})
		
	end,
    mnesia:transaction(F).
check_channel(ChannelName)->
    F = fun()->
		mnesia:read({channel,ChannelName})
	end,
    mnesia:transaction(F).

extract_nick(List,Nick)->
    extract_nick(List,Nick,[]).
extract_nick([],_Nick,Ack)->
    Ack;
extract_nick([H|T], Nick,Ack) ->
    if
	H == Nick ->
	    [T|Ack];
	true ->
	    extract_nick(T,Nick,[H|Ack])
    end.

part_channel(ChannelName, Nick, Socket)->
    F = fun() ->
		[Channel] = mnesia:wread({channel, ChannelName}),
		[User] = mnesia:wread({user,Socket}),
		
		{_, [{_, _Name, NickList, _Topic}]} = check_channel(ChannelName),
		NewNickList = lists:delete(Nick,NickList),
		{_,[{_,_,_,_,_,_,_,ChannelList}]} = check_socket(Socket),
		NewChannelList = lists:delete(ChannelName,ChannelList),

		mnesia:write(Channel#channel{users=NewNickList}),
		mnesia:write(User#user{channel_list=NewChannelList})
	end,
    mnesia:transaction(F).

change_channel_nick(ChannelName,NewNick,Socket)->
    F = fun() ->
		[Channel] = mnesia:wread({channel, ChannelName}),
		{_, [{_, _Name, NickList, _Topic}]} = check_channel(ChannelName),
		{_,Nick} = get_nick(Socket),
		{Status, _Nick} = lists:keysearch(Nick,2,NickList),
		NewNickList = lists:keyreplace(Nick,2,NickList,{Status,NewNick}),
		mnesia:write(Channel#channel{users= NewNickList})
	end,
    mnesia:transaction(F).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               EUnit database test                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_head([])->
    [];
get_head([H|_T])->
    H.

%% update_list_test()->
%%    Channel = #channel{id="ost",users = [{[o,a], "mumin"}, {[r,a],"hemulen"}]},
%%    TestList = update_list(Channel,{[o],"korv"}),
%%    {[H|_T],Name} = get_head(TestList),
%%    [?assertEqual(H,o),
%%    ?assertEqual(Name,"korv")
%%    ].

%% database_test()->
%%    create_db([node()]),
%%    insert_user("Perkson",12321,"servername"),
%%    {_,User} = query_database("Perkson"),
%%    ?assertEqual(User,[{user,"Perkson","pettsson",["socker","salt"],12321,"servername"}]).

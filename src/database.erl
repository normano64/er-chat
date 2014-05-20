%% @author Sam, Mattias,Ludwing, Per och Tomas
%% @doc Database
-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-record(channel,{id, users, topic}).
-record(user,{socket, user, nick, server,hostent, realname, channel_list}). %%add channel list
-record(server,{id,servername,socket,active}). %%what additional parameters?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Database functions                                          %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%   @doc create_db,  This function starts the database nodes and creates two tables; user and channel.
%%


create_db()->
    ListNodes = [node()],
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{type,set}]),
    mnesia:create_table(channel,[{attributes,record_info(fields,channel)},{disc_copies,ListNodes},{type,set}]),
    mnesia:create_table(server,[{attributes, record_info(fields,server)},{disc_copies,ListNodes},{type,set}]).


%% @doc	traverse_table_and_show, This function simply traverse the desired table in the database and prints it in the shell. 
%%


traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true ->
	    mnesia:foldl(Iterator,[],Table_name);
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



%% @doc insert_user, This function inserts a user in the database.
%% It's parameters are the values of the columns in the table 
%% The user can then be then later be found with it's unique socket
%%


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
		LowerCase = string:to_lower(binary:bin_to_list(Nick)),
		Match = mnesia:match_object({user,'_','_',{LowerCase,'_'},'_','_','_','_'}),
		Match
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



%% @doc	insert_channel,This function creates a channel where the id is the parameter ChannelName with only one user (specified as in the parameter Nick)
%% The name of the topic is specified in the last parameter Topic
delete_channel(Channel)->
    F = fun()->
		mnesia:delete({channel,Channel})
	end,
    mnesia:transaction(F).

insert_channel(ChannelName, {Status,Nick}, Topic) ->
    Data = #channel{id = ChannelName,users = [{Status,Nick}], topic = Topic},
    F = fun()->
		mnesia:write(Data),
                {_,[{_,Socket, _User, _Nick, _Server, _Hostent, _RealName, ChannelList}]} = check_nick(Nick),

		[User] = mnesia:wread({user, Socket}),
		NewChannelList = [ChannelName | ChannelList],
		mnesia:write(User#user{channel_list = NewChannelList}) 
	end,
    mnesia:transaction(F).

update_list(#channel{id = _Id,users = UserList}, User)->
    [User | UserList].



%% @doc	join_channel, This function links a user to a channel by using a channel name, a nick name and a user id (Socket)
%%  In the channel list, the nick is added and in the user's channel list, the name of the channel is added
%%
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

set_topic(ChannelName, Topic)->
    F = fun()->
		[Channel]= mnesia:wread({channel,ChannelName}),
		mnesia:write(Channel#channel{topic = Topic})
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

%% @doc	part_channel, This function removes a user from a channel by using
%%  A ChannelName to identify the channel
%%  A nickname to remove from it's nick-list
%%  And finally, the socket of the user to remove the link (the channel has to be removed from the user's channel list)
%%


part_channel(ChannelName, Nick, Socket)->
    F = fun() ->
		[Channel] = mnesia:wread({channel, ChannelName}),
		[User] = mnesia:wread({user,Socket}),
		
		{_, [{_, _Name, NickList, _Topic}]} = check_channel(ChannelName),
		NewNickList = lists:keydelete(Nick,2,NickList),
		if 
		    NewNickList == [] ->
			delete_channel(ChannelName);
		    true ->
			{_,[{_,_,_,_,_,_,_,ChannelList}]} = check_socket(Socket),
			NewChannelList = lists:delete(ChannelName,ChannelList),

			mnesia:write(Channel#channel{users=NewNickList}),
			mnesia:write(User#user{channel_list=NewChannelList})
		end
	   end,
    mnesia:transaction(F).

change_channel_nick(ChannelName,NewNick,Socket)->
    F = fun() ->
		[Channel] = mnesia:wread({channel, ChannelName}),
		{_, [{_, _Name, NickList, _Topic}]} = check_channel(ChannelName),
		{_,{_,Nick}} = get_nick(Socket),
		{value,{Status, _Nick}} = lists:keysearch(Nick,2,NickList),
		NewNickList = lists:keyreplace(Nick,2,NickList,{Status,NewNick}),
		mnesia:write(Channel#channel{users= NewNickList})
	end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Server Functions                                            %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc	insert_server, This function adds a server where the id is the parameter ServerName. The Server can be found with it's unique socket.

insert_server(Id,Servername,Socket,Active) ->
    Data = #server{id=Id,servername=Servername, socket=Socket, active=Active},
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

%% @doc delete_server. This function deletes a server named in the parameter Server.

delete_server(Server)->
    F = fun()->
		mnesia:delete({server,Server})
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


reset()->
	create_db(),
	delete_table_db(user),
	delete_table_db(channel),
	stop().

database_functions_test()->
	[reset(),
	?assertExit({aborted,{node_not_running,nonode@nohost}},traverse_table_and_show(user)),
	start(),	
	?assertExit({aborted,{no_exists,user}},traverse_table_and_show(user)),
	?assertExit({aborted,{no_exists,channel}},traverse_table_and_show(channel)),
	delete_table_db(user),
	delete_table_db(channel),
	stop(),
	?assertMatch({atomic,ok},create_db()),
	?assertMatch({aborted,{already_exists,channel}}, create_db()),
	?assertMatch([] , traverse_table_and_show(user)),
	?assertMatch({atomic,ok} , insert_user(123,kalle,stekare,servername,host,"Kalle Anka")),
	?assertMatch({atomic,ok} , delete_table_db(user)),
	?assertMatch({atomic,ok} , delete_table_db(channel)),
	stop(),{test_passed,ok}].

nick_test()->
	[reset(),
	create_db(),
	?assertMatch({atomic,ok}, insert_user(123,kalle,stekare,servername,host,"Kalle Anka")),
	
	?assertMatch({atomic,[{user,123,kalle,stekare,servername,host,"Kalle Anka",[]}]}  , check_socket(123)),
	?assertMatch({atomic,[{user,123,kalle,stekare,servername,host,"Kalle Anka",[]}]}  , check_nick(stekare)),
	?assertMatch({atomic,ok} , update_nick(123, flygare)),
	?assertMatch({atomic,[{user,123,kalle,flygare,servername,host,"Kalle Anka",[]}]}  , check_socket(123)),
	?assertMatch({atomic,[{user,123,kalle,flygare,servername,host,"Kalle Anka",[]}]}  , check_nick(flygare)),
	delete_table_db(user),
	delete_table_db(channel),
	stop(),{test_passed,ok}].


channel_test()->
    delete_table_db(user),
    delete_table_db(channel),
    delete_table_db(server),
    create_db(),
    insert_user(<<"Socket1">>,<<"UserKalle">>,{"nick1",<<"NiCk1">>},<<"Servername1">>,<<"Hostname1">>,<<"Realname1">>),
    insert_user(<<"Socket2">>,<<"UserRandom">>,{"randomuser",<<"RandomUser">>},<<"Servername2">>,<<"Hostname2">>,<<"Realname2">>),
    insert_channel(<<"#Channel1">>,{<<"@">>,<<"NiCk1">>},<<"Topic">>),
    join_channel(<<"#Channel1">>,{<<"">>,<<"RandomUser">>},<<"Socket2">>),

    [?assertMatch({_,[{channel,<<"#Channel1">>,[{<<"">>,<<"RandomUser">>},{<<"@">>,<<"NiCk1">>}],<<"Topic">>}]},check_channel(<<"#Channel1">>)),
     ?assertMatch({_,[]},check_channel(<<"NotExistingChannel">>)),
     ?assertMatch({atomic,_},part_channel(<<"#Channel1">>,<<"RandomUser">>,<<"Socket2">>)),
     ?assertMatch({_,[{channel,<<"#Channel1">>,[{<<"@">>,<<"NiCk1">>}],<<"Topic">>}]},check_channel(<<"#Channel1">>)),
     ?assertMatch({_,[{user,_,_,_,_,_,_,[]}]},check_socket(<<"Socket2">>)),
     ?assertMatch({_,[{user,_,_,_,_,_,_,[<<"#Channel1">>]}]},check_socket(<<"Socket1">>))].







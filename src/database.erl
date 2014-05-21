%% coding: latin-1
%% @author Sam, Mattias,Ludwing, Per och Tomas
%% @doc Database module implemented using mnesia
-include_lib("eunit/include/eunit.hrl").
-module(database).
-compile(export_all).
-export_type([any/0]).
-record(channel,{id, users, topic}).
-record(user,{socket, user, nick, server,hostent, realname, channel_list}).
-record(server,{id,servername,socket,active}). %%what additional parameters?

%% @doc Generated a new database
-spec create_db()->atom().

create_db()->
    ListNodes = [node()],
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{type,set}]),
    mnesia:create_table(channel,[{attributes,record_info(fields,channel)},{disc_copies,ListNodes},{type,set}]),
    mnesia:create_table(server,[{attributes, record_info(fields,server)},{disc_copies,ListNodes},{type,set}]).

%% @doc Prints out every element in Table from the database.
-spec traverse_table_and_show(Table_name::atom())-> list().

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

%% @doc Deletes the table Table from the database
-spec delete_table_db(Table::atom())-> atom().

delete_table_db(Table)->
    mnesia:delete_table(Table).

%% @doc Starts the database if it has been stopped.
start()->
    mnesia:start().
%% @doc Stops the databse from running.
stop()-> 
    mnesia:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Socket/nick functions                                       %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Inserts a new user in the database.
insert_user(Socket,User,Nick,Server,Hostent,Realname)->
    Data = #user{socket=Socket,user=User,nick=Nick, server=Server,hostent=Hostent, realname=Realname, channel_list = []},
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

%% @doc Query the database using the socket of a users as key.
check_socket(Socket)->
    F = fun() ->
		SocketDb = mnesia:read(user,Socket),
		SocketDb
	end,
    mnesia:transaction(F).

%% @doc Query the database using the nick of a users as a key.
check_nick(Nick)->
    F = fun() ->
		LowerCase = string:to_lower(binary:bin_to_list(Nick)),
		Match = mnesia:match_object({user,'_','_',{LowerCase,'_'},'_','_','_','_'}),
		Match
	   end,
    mnesia:transaction(F).

%% @doc Extracts the nick from a user using Socket as a key.
get_nick(Socket)->
    F = fun()->
		{_,List} = check_socket(Socket),
		find_nick(get_head(List))
	end,
    mnesia:transaction(F).

%% @doc Updates the Nick os a user with id Socket.
update_nick(Socket, Nick)->
    F = fun()->
		[P] = mnesia:wread({user,Socket}),
		mnesia:write(P#user{nick = Nick})
	end,
    mnesia:transaction(F).

%% @doc Updates information of a user where the key is Socket.
update_user(Socket, User, Realname)->
    F = fun()->
		[P] = mnesia:wread({user,Socket}),
		mnesia:write(P#user{user=User,realname=Realname})
	end,
    mnesia:transaction(F).

%% @doc Deletes the element with Socket as ID from the database.
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

%% @doc Deletes a channel from the database.
delete_channel(Channel)->
    F = fun()->
		mnesia:delete({channel,Channel})
	end,
    mnesia:transaction(F).

%% @doc Creates a new instance of a channel in the database.
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


%% @doc Adds a users Nick to the channel ChannelName.
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

%% @doc Changes the topic in ChannelName.
set_topic(ChannelName, Topic)->
    F = fun()->
		[Channel]= mnesia:wread({channel,ChannelName}),
		mnesia:write(Channel#channel{topic = Topic})
	end,
    mnesia:transaction(F).

%% @doc Query the database for ChannelName and return information about the channel.
check_channel(ChannelName)->
    F = fun()->
		mnesia:read({channel,ChannelName})
	end,
    mnesia:transaction(F).


%% @doc Removes a user from a channel and the channel from the users channelList.
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

%% @doc Change the nick of a user in a channel. ChannelName is the name of the
%%      channel we want to find the user in and NewNick is the Nick we want to change
%%      too and Socket is the SocketId of the user.
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

%% @hidden
get_first_channel(Tab)->
   F = fun()-> 
	       First = mnesia:first(Tab),
	       First
       end,
    mnesia:transaction(F).
%% @hidden
get_next_channel(Tab,Key)->
    F = fun()->
		Next = mnesia:next(Tab,Key),
		Next
	end,
    mnesia:transaction(F).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Server Functions                                            %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc This function adds a server where the id is the parameter ServerName, Socket is
%%      the socket of the server and Active is if the server is active or not. The Server 
%%      can be found with it's unique id.
insert_server(ServerId,Servername,Socket,Active) ->
    Data = #server{id=ServerId,servername=Servername, socket=Socket, active=Active},
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

%% @doc This function deletes a server names as Server in the server table from the database.
delete_server(ServerId)->
    F = fun()->
		mnesia:delete({server,ServerId})
	end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               EUnit database test                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
reset()->
	create_db(),
	delete_table_db(user),
	delete_table_db(channel),
	stop().

%% @hidden
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

%% @hidden
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

%% @hidden
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                                    Help functions                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
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

%% @hidden
get_head([])->
    [];
get_head([H|_T])->
    H.

%% @hidden
update_list(#channel{id = _Id,users = UserList}, User)->
    [User | UserList].

%% @hidden
find_channellist({_,_,_,_,_,_,_,ChannelList})->
    ChannelList.

%% @hidden
find_nick([])->
    [];
find_nick({_,_,_,Nick,_,_,_,_})->
    Nick.

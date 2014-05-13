
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		create_db
%
%  This function starts the database nodes and creates two tables; user and channel.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_db()->
    ListNodes = [node()],
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{type,set}]),
    mnesia:create_table(channel,[{attributes,record_info(fields,channel)},{disc_copies,ListNodes},{type,set}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	traverse_table_and_show
%
% This function simply traverse the desired table in the database
% and prints it in the shell. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     insert_user
%
% This function inserts a user in the database.
% It's parameters are the values of the columns in the table 
% The user can then be then later be found with it's unique socket
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	insert_channel
%
%	This function creates a channel where the id is the parameter ChannelName with only one user (specified as in the parameter Nick)
%	The name of the topic is specified in the last parameter Topic
%
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_channel(ChannelName, Nick, Topic) ->
    Data = #channel{id = ChannelName,users = [Nick], topic = Topic},
    F = fun()->
		mnesia:write(Data),

		%Below is the original code
		%{_,[{_,Socket, _User, _Nick, _Server, _Hostent, _RealName, ChannelList}]} = check_nick(Nick),

		%Below is not the original code, it is a working version
		{_,[{_,Socket,_User,_Nick,_Server,_Hostent,_RealName,ChannelList}|_]}= check_nick(Nick), %We need to find the Socket of the nick 
		[User] = mnesia:wread({user, Socket}), %here we extract the user from the table user
		NewChannelList = [ChannelName | ChannelList],
		mnesia:write(User#user{channel_list = NewChannelList}) 
	end,
    mnesia:transaction(F).

update_list(#channel{id = _Id,users = UserList}, User)->
    [User | UserList].


%%%%%%%%%%%%%%%%%
%	join_channel
%
%	This function links a user to a channel by using a channel name, a nick name and a user id (Socket)
%	In the channel list, the nick is added and in the user's channel list, the name of the channel is added
%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%
%	part_channel
%	
%	This function removes a user from a channel by using
%   A ChannelName to identify the channel
%	A nickname to remove from it's nick-list
%	And finally, the socket of the user to remove the link (the channel has to be removed from the user's channel list)
%	
%
%%%%%%%%%%%%%%%%%%


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
		{value,{Status, _Nick}} = lists:keysearch(Nick,2,NickList),
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

test_all()->
{test_passed,ok} = test_database(),
{test_passed,ok} = test_nick(),
{test_passed,ok} = test_channel().

test_database()->

	delete_table_db(user),
	delete_table_db(channel),
	stop(),
	try traverse_table_and_show(user) of
		E ->
			exit({test_failed_node_running,E})
	catch
		exit:{aborted,{node_not_running,nonode@nohost}} -> ok
	end,
	start(),	
	try traverse_table_and_show(user) of
		_  ->
			exit(test_failed_table_exist)
	catch
		exit: {aborted,{no_exists,user}} ->ok
	end,
	{atomic,ok} = create_db(),
	{aborted,{already_exists,channel}}= create_db(),
	[] = traverse_table_and_show(user),
	{atomic,ok} = insert_user(123,kalle,stekare,servername,host,"Kalle Anka"),
	{atomic,ok} = delete_table_db(user),
	{atomic,ok} = delete_table_db(channel),
	stop(),
	{test_passed,ok}.

test_nick()->
	create_db(),
	{atomic,ok} = insert_user(123,kalle,stekare,servername,host,"Kalle Anka"),
	User = {user,123,kalle,stekare,servername,host,"Kalle Anka",[]},
	User2 = {user,123,kalle,flygare,servername,host,"Kalle Anka",[]},
	{atomic,[User]}  = check_socket(123),
	{atomic,[User]}  = check_nick(stekare),
	{atomic,ok} = update_nick(123, flygare),
	{atomic,[User2]}  = check_socket(123),
	{atomic,[User2]}  = check_nick(flygare),
	delete_table_db(user),
	delete_table_db(channel),
	stop(),
	{test_passed,ok}.


test_channel()->
	delete_table_db(user),
	delete_table_db(channel),
	create_db(),
	insert_user(123,kalle,stekare,servername,host,"Kalle Anka"),
	{atomic, ok} = insert_channel(computers, stekare,keyboards),
	{_,[{_,computers,[stekare],keyboards}]} = check_channel(computers),
	{atomic, ok} = part_channel(computers, stekare, 123),
	{_,[{_,computers,[],keyboards}]} = check_channel(computers),
	{test_passed,ok}.







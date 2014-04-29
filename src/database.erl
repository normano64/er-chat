-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(user,{id,password,channels,socket,server}).
-record(channel,{id, users}).

create_db(ListNodes)->
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,ListNodes},{type,set}]).

insert_user(Id,Password,Channels,Socket,Server)->
    Data = #user{id=Id, password=Password, channels = Channels, socket=Socket, server=Server},
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

insert_channel(ChannelName, User) ->
    Data = #channel{id = ChannelName,users = [User]},
    F = fun()->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

update_list(#channel{id = _Id,users = UserList}, User)->
    [User | UserList].

join_channel(ChannelId, User)->
    F = fun()->
		[Channel]= mnesia:read(ChannelId),
		mnesia:write(Channel#channel{id = ChannelId,users=update_list(Channel,User)})
	end,
    mnesia:transaction(F).

query_database(Id)->
    F = fun() ->
		mnesia:read(user,Id)
	end,
    mnesia:transaction(F).

start()->
    mnesia:start().
stop()->
    mnesia:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                               EUnit database test
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_touple([H|_T])->
    H.
update_list_test()->
    Channel = #channel{id="ost",users = [{[o,a], "mumin"}, {[r,a],"hemulen"}]},
    TestList = update_list(Channel,{[o],"korv"}),
    {[H|_T],Name} = get_touple(TestList),
    [?assertEqual(H,o),
    ?assertEqual(Name,"korv")
    ].

-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(user,{socket, id, server}).
-record(channel,{id, users}).

create_db(ListNodes)->
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,ListNodes},{type,set}]).

insert_user(Socket,Id,Server)->
    Data = #user{socket=Socket,id=Id, server=Server},
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

check_socket(Socket)->
    F = fun() ->
		mnesia:read(user,Socket)
	end,
    mnesia:transaction(F).

query_user(Id)->
    F = fun() ->
		User = mnesia:read(user,Id),
		User
	end,
    mnesia:transaction(F).

check_nick(Nick)->
    F = fun() ->
		mnesia:match_object({user,'_',Nick,'_'})
	end,
    mnesia:transaction(F).

get_nick(_Socket)->
    <<"localnick">>.

delete_user(Id)->
    F = fun()->
		mnesia:delete({user,Id})
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


get_head([H|_T])->
    H.
update_list_test()->
    Channel = #channel{id="ost",users = [{[o,a], "mumin"}, {[r,a],"hemulen"}]},
    TestList = update_list(Channel,{[o],"korv"}),
    {[H|_T],Name} = get_head(TestList),
    [?assertEqual(H,o),
    ?assertEqual(Name,"korv")
    ].

database_test()->
    create_db([node()]),
    insert_user("Perkson",12321,"servername"),
    {_,User} = query_database("Perkson"),
    ?assertEqual(User,[{user,"Perkson","pettsson",["socker","salt"],12321,"servername"}]).

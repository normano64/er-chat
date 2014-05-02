-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(user,{socket, nick, server}).
-record(channel,{id, users}).

create_db(ListNodes)->
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,ListNodes},{type,set}]).

insert_user(Socket,Nick,Server)->
    Data = #user{socket=Socket,nick=Nick, server=Server},
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
		mnesia:match_object({user,'_',Nick,'_'})
	end,
    mnesia:transaction(F).
find_nick([])->
    [];
find_nick({_,_,Nick,_})->
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

delete_socket(Socket)->
    F = fun()->
		mnesia:delete({user,Socket})
	end,
    mnesia:transaction(F).
start()->
    mnesia:start().
stop()->
    mnesia:stop().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                               EUnit database test
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_head([])->
    [];
get_head([H|_T])->
    H.
%%update_list_test()->
  %%  Channel = #channel{id="ost",users = [{[o,a], "mumin"}, {[r,a],"hemulen"}]},
  %%  TestList = update_list(Channel,{[o],"korv"}),
  %%  {[H|_T],Name} = get_head(TestList),
  %%  [?assertEqual(H,o),
  %%  ?assertEqual(Name,"korv")
  %%  ].

%%database_test()->
  %%  create_db([node()]),
   %% insert_user("Perkson",12321,"servername"),
   %% {_,User} = query_database("Perkson"),
   %% ?assertEqual(User,[{user,"Perkson","pettsson",["socker","salt"],12321,"servername"}]).

-module(database).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(user,{socket, user, nick, server,hostent, realname}).
-record(channel,{id, users}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               Database functions                                          %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_db(ListNodes)->
    mnesia:create_schema(ListNodes),
    mnesia:start(),
    mnesia:create_table(user,[{attributes,record_info(fields,user)},{disc_copies,ListNodes},{type,set}]).

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
    Data = #user{socket=Socket,user=User,nick=Nick, server=Server,hostent=Hostent, realname=Realname},
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
		Found = mnesia:match_object({user,'_','_',Nick,'_','_','_'}),
		Found
	end,
    mnesia:transaction(F).

find_nick([])->
    [];
find_nick({_,_,_,Nick,_,_,_})->
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
%                                                                                           %
%                               EUnit database test                                         %
%                                                                                           %
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

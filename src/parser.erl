-module(parser).
-include_lib("eunit/include/eunit.hrl").
%%-compile(export_all).
-export([parse/1, loop/2]).
-define(SLASHR,13).
-define(SPACE,32).
-define(COLON,58).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                                  Parser functions                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(UserPid,OtherPid)->
    receive {ok,Message}-> 
	    case parse(Message) of
		{<<"NICK">>,List}-> 
		    OtherPid ! {nick,List},
		    loop(UserPid,OtherPid);
		{<<"USER">>,List} ->
		    UserPid ! {user,List},
		    loop(UserPid,OtherPid)
		end
    end.


parse(Bitstring) ->
    Bitstring_nor = << <<Bitstring_nor>> || <<Bitstring_nor>> <= Bitstring, Bitstring_nor =/= ?SLASHR>>,
    [Bitstring_noColon|Colonpart] = binary:split(Bitstring_nor, <<?SPACE,?COLON>>, [global]),
    Bitlist = binary:split(Bitstring_noColon, <<?SPACE>>, [global]),
    Bitlist_complete = Bitlist ++ Colonpart,
    [Command|Parameters] = Bitlist_complete,
    {Command,Parameters}.

%%Notice that the current implementation cannot handle multiple spaces following eachother and will generate empty parameters.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               EUnit parser test                                           %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parser_test() ->
    Bin1 = <<72,?SLASHR,69,?SLASHR,76,?SLASHR,76,?SLASHR,79>>,
    Bin2 = <<72,69,?SPACE,76,76,79>>,
    Bin3 = <<72,?SPACE,69,?SPACE,76,?SPACE,76,?SPACE,79>>,
    Bin4 = <<?SLASHR,72,69,?SLASHR,?SPACE,76,76,?SLASHR,79,?SPACE>>,
    ?assertEqual(parse(Bin1),{<<"HELLO">>,[]}),
    ?assertEqual(parse(Bin2),{<<"HE">>,[<<"LLO">>]}),
    ?assertEqual(parse(Bin3),{<<"H">>,[<<"E">>,<<"L">>,<<"L">>,<<"O">>]}),
    ?assertEqual(parse(Bin4),{<<"HE">>,[<<"LL">>,<<"O">>,<<>>]}).

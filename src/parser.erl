%% @author Sam, Mattias, Ludwing, Per och Tomas
%% @doc Parser
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
		    loop(UserPid,OtherPid);
		{<<"PING">>,List} ->
		    OtherPid ! {ping, List},
		    loop(UserPid, OtherPid);
                {<<"QUIT">>,List} ->
		    OtherPid ! {quit, List},
		    loop(UserPid, OtherPid);
                {Command,_} ->
                    OtherPid ! {unknown, Command},
                    loop(UserPid, OtherPid)
		end
    end.

%% @doc The parse function takes a binary string and formates it to a tuple {Command/binary, [Parameters/binary]}.
%%
%%== Example ==
%% parse(<<"USER GUEST 0 * :Ronald Mcdonald">>)
%% {<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<":Ronald Mcdonald">>]}
%%
%%WEAKNESSES OF PARSE
%%It cannot handle multiple spaces following eachother and will then generate empty parameters <<>>.
%%If a colon isn't preceded by a space it will not be placed in Colonpart.
%%Everything after " :" will be placed in the Colonpart and will not acted upon anymore, even another " :".




parse(Bitstring) ->
    Bitstring_nor = << <<Bitstring_nor>> || <<Bitstring_nor>> <= Bitstring, Bitstring_nor =/= ?SLASHR>>,
    [Bitstring_noColon|Colonpart] = binary:split(Bitstring_nor, <<?SPACE,?COLON>>, [global]),
    Bitlist = binary:split(Bitstring_noColon, <<?SPACE>>, [global]),
    Bitlist_complete = Bitlist ++ Colonpart,
    [Command|Parameters] = Bitlist_complete,
    {Command,Parameters}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               EUnit parser test                                           %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parser_test() ->
    Bin1 = <<72,?SLASHR,69,?SLASHR,76,?SLASHR,76,?SLASHR,79>>,
    Bin2 = <<72,?SPACE,69,?SPACE,76,?SPACE,76,?SPACE,79>>,
    Bin3 = <<?SLASHR,72,69,?SLASHR,?SPACE,76,76,?SLASHR,79,?SPACE>>,
    Bin4 = <<"USER guest 0 * :Ronald Mcdonald">>,
    Bin5 = <<"USER guest 0 *:Ronald Mcdonald">>,
    ?assertEqual(parse(Bin1),{<<"HELLO">>,[]}),
    ?assertEqual(parse(Bin2),{<<"H">>,[<<"E">>,<<"L">>,<<"L">>,<<"O">>]}),
    ?assertEqual(parse(Bin3),{<<"HE">>,[<<"LL">>,<<"O">>,<<>>]}),
    ?assertEqual(parse(Bin4),{<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<":Ronald Mcdonald">>]}),
    ?assertEqual(parse(Bin5),{<<"USER">>,[<<"guest">>,<<"0">>,<<"*:Ronald">>,<<"Mcdonald">>]}).

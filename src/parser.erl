%% @author Sam, Mattias, Ludwing, Per och Tomas
%% @doc Parser
-module(parser).
-include_lib("eunit/include/eunit.hrl").
%%-compile(export_all).
-export([parse/1,loop/2]).
-define(SLASHR,13).
-define(SPACE,32).
-define(COLON,58).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                                  Parser functions                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(UserPid,OtherPid) ->
    receive {ok,Message} ->
            case parse(Message) of
                {_,<<"NICK">>,List} ->
                    OtherPid ! {nick,List},
                    loop(UserPid,OtherPid);
                {_,<<"USER">>,List} ->
                    UserPid ! {user,List},
                    loop(UserPid,OtherPid);
                {_,<<"PING">>,List} ->
                    OtherPid ! {ping,List},
                    loop(UserPid,OtherPid);
                {_,<<"PONG">>,List} ->
                    OtherPid ! {pong,List},
                    loop(UserPid,OtherPid);
                {_,<<"QUIT">>,List} ->
                    OtherPid ! {quit,List},
                    loop(UserPid,OtherPid);
                {_,<<"JOIN">>,List} ->
                    OtherPid ! {join,List},
                    loop(UserPid,OtherPid);
                {_,<<"PRIVMSG">>,List} ->
                    OtherPid ! {privmsg,List},
                    loop(UserPid,OtherPid);
                {_,<<"PART">>,List} ->
                    OtherPid ! {part,List},
                    loop(UserPid,OtherPid);
                {_,<<"WHOIS">>,List} ->
                    OtherPid ! {whois,List},
                    loop(UserPid,OtherPid);
                {_,Command,_} ->
                    OtherPid ! {unknown,Command},
                    loop(UserPid,OtherPid)
                end
    end.

%% @doc The parse function takes a binary string and formates it to a tuple {Prefix/binary,Command/binary,[Parameters/binary]}.
%%
%%== Example ==
%% parse(<<"USER GUEST 0 * :Ronald Mcdonald">>)
%% {noprefix,<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<":Ronald Mcdonald">>]}
%%
%%WEAKNESSES OF PARSE
%%It cannot handle multiple spaces following eachother and will then generate empty parameters <<>>.
%%If a colon isn't preceded by a space it will not be placed in Colonpart.
%%Everything after " :" will be placed in the Colonpart and will not acted upon anymore,even another " :".
%%Handling of prefix is not complete and bugfree.

parse(Bitstring) ->
%%Remove Carriage Return.
    Bitstring_noR = << <<Bitstring_noR>> || <<Bitstring_noR>> <= Bitstring,Bitstring_noR =/= ?SLASHR>>,
%%Trims of the prefix.
    case Bitstring_noR of
        <<$:,Rest/binary>> ->
            IsMatch1 = binary:match(Rest,<<?SPACE>>),
	    if
		IsMatch1 =:= nomatch ->
		    Prefix = badprefix, %%Bad case, need to overlook design!
		    Bitstring_noPrefix = <<>>;
		true ->
		    {Pos1,_} = IsMatch1,
		    <<Prefix:Pos1/binary,_Unused1:1/binary,Bitstring_noPrefix/binary>> = Rest	    
		end;
        _ ->
            Prefix = noprefix,
            Bitstring_noPrefix = Bitstring_noR
        end,
    %%Splits the Bitstring at the Colonpart if it exists.
    IsMatch2 = binary:match(Bitstring_noPrefix,<<?SPACE,?COLON>>),
    if
	IsMatch2 =:= nomatch ->
	    Bitstring_noColon = Bitstring_noPrefix,
	    Colonpart = [];
	true ->
	    {Pos2,_} = IsMatch2,
	    <<Bitstring_noColon:Pos2/binary,_Unused2:2/binary,Colonbit/binary>> = Bitstring_noPrefix,
	    Colonpart = [Colonbit]
    end,
    %%Splits the rest at spaces.
    Bitlist = binary:split(Bitstring_noColon,<<?SPACE>>,[global]),
    %%Appends all the parts and return.
    Bitlist_complete = Bitlist ++ Colonpart,
    [Command|Parameters] = Bitlist_complete,
    {Prefix,Command,Parameters}.

%% [Bitstring_noColon|Colonpart] = binary:split(Bitstring_noPrefix,<<?SPACE,?COLON>>,[global]),


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
    Bin6 = <<":Prefix USER guest 0 * :Ronald Mcdonald">>,
    ?assertEqual(parse(Bin1),{noprefix,<<"HELLO">>,[]}),
    ?assertEqual(parse(Bin2),{noprefix,<<"H">>,[<<"E">>,<<"L">>,<<"L">>,<<"O">>]}),
    ?assertEqual(parse(Bin3),{noprefix,<<"HE">>,[<<"LL">>,<<"O">>,<<>>]}),
    ?assertEqual(parse(Bin4),{noprefix,<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<":Ronald Mcdonald">>]}),
    ?assertEqual(parse(Bin5),{noprefix,<<"USER">>,[<<"guest">>,<<"0">>,<<"*:Ronald">>,<<"Mcdonald">>]}),
    ?assertEqual(parse(Bin6),{<<"Prefix">>,<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<":Ronald Mcdonald">>]}).

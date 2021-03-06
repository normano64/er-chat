%% coding: latin-1
%% @author Sam, Mattias, Ludwing, Per och Tomas
%% @doc Parser module, parses the incomming messages.
-module(parser).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1,loop/2]).
-define(SLASHR,13).
-define(SPACE,32).
-define(COLON,58).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                                  Parser functions                                         %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc This is the main function used to deceipher an incomming message.
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
                {_,<<"TOPIC">>,List} ->
                    OtherPid ! {topic,List},
                    loop(UserPid,OtherPid);
		{_,<<"INVITE">>,List} ->
		    OtherPid ! {invite, List},
		    loop(UserPid, OtherPid);
                {_,<<"MODE">>,List} ->
		    OtherPid ! {mode, List},
		    loop(UserPid, OtherPid);
		{_,<<"KICK">>,List}->
		    OtherPid ! {kick, List},
		    loop(UserPid,OtherPid);
		{_,<<"NAMES">>,List}->
		    OtherPid ! {names, List},
		    loop(UserPid,OtherPid);
		{_,<<"WHO">>,List}->
		    OtherPid ! {who, List},
		    loop(UserPid,OtherPid);
		{_,<<"LIST">>,List}->
		    OtherPid ! {list, List},
		    loop(UserPid, OtherPid);
%% SERVER COMMANDS
%% NICK,JOIN och QUIT ska in på servercommands		
%% Eftersom de har motsvarande på client calls så måste de skiljas på.
	
		{Prefix,<<"NJOIN">>, List} ->
		    OtherPid ! {Prefix, njoin, List},
		    loop(UserPid, OtherPid);
		{_,<<"SERVER">>, List} ->         
		    OtherPid ! {server, List},
		    loop(UserPid, OtherPid);
		{Prefix, <<"SQUIT">>, List}->
		    OtherPid ! {Prefix, squit, List},
		    loop(UserPid, OtherPid);
		{Prefix, <<"SERVICE">>, List}->   
		    OtherPid ! {Prefix, service, List},
		    loop(UserPid, OtherPid);
		{Prefix, <<"PASS">>, List}->
		    OtherPid ! {Prefix, pass, List},
		    loop(UserPid, OtherPid);
		
		{_,Command,_} ->
                    OtherPid ! {unknown,Command},
                    loop(UserPid,OtherPid)
                end
    end.



%% @doc The parse function takes a binary string and formates it to a tuple {Prefix/binary,Command/binary,[Parameters/binary]}.
%%      If a colon isn't preceded by a space it will not be placed in Colonpart.
%%      Everything after " :" will be placed in the Colonpart and will not acted upon anymore,even another " :".

parse(Bitstring) ->
%%Remove Carriage Return.
    Bitstring_noR = << <<Bitstring_noR>> || <<Bitstring_noR>> <= Bitstring,Bitstring_noR =/= ?SLASHR>>,
%%Trims of the prefix.
    case Bitstring_noR of
        <<$:,Rest/binary>> ->
            IsMatch1 = binary:match(Rest,<<?SPACE>>),
	    if
		IsMatch1 =:= nomatch ->
		    Prefix = badprefix, %%Need to overlook design?
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                           %
%                               EUnit parser test                                           %
%                                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
parser_test() ->
    Bin1 = <<72,?SLASHR,69,?SLASHR,76,?SLASHR,76,?SLASHR,79>>,
    Bin2 = <<72,?SPACE,69,?SPACE,76,?SPACE,76,?SPACE,79>>,
    Bin3 = <<?SLASHR,72,69,?SLASHR,?SPACE,76,76,?SPACE,79,?SPACE>>,
    Bin4 = <<"USER guest 0 * :Ronald Mcdonald">>,
    Bin5 = <<"USER guest 0 *:Ronald Mcdonald">>,
    Bin6 = <<":Prefix USER guest 0 * :Ronald Mcdonald">>,
    [?assertEqual(parse(Bin1),{noprefix,<<"HELLO">>,[]}),
    ?assertEqual(parse(Bin2),{noprefix,<<"H">>,[<<"E">>,<<"L">>,<<"L">>,<<"O">>]}),
    ?assertEqual(parse(Bin3),{noprefix,<<"HE">>,[<<"LL">>,<<"O">>,<<>>]}),
    ?assertEqual(parse(Bin4),{noprefix,<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<"Ronald Mcdonald">>]}),
    ?assertEqual(parse(Bin5),{noprefix,<<"USER">>,[<<"guest">>,<<"0">>,<<"*:Ronald">>,<<"Mcdonald">>]}),
    ?assertEqual(parse(Bin6),{<<"Prefix">>,<<"USER">>,[<<"guest">>,<<"0">>,<<"*">>,<<"Ronald Mcdonald">>]})].

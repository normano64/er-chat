-module(parser).
%% -include_lib("eunit/include/eunit.hlr").
%%-compile(export_all).
-export([parse/1, loop/2]).
-define(SLASHR,13).
-define(SPACE,32).

loop(UserPid,_OtherPid)->
    receive {ok,Message}-> 
	    case parse(Message) of
		{<<"NICK">>,List}-> 
		    UserPid ! {nick,List},
		    loop(UserPid,_OtherPid) end
    end.

parse(Bitstring) ->
    Bitstring_nor = << <<Bitstring_nor>> || <<Bitstring_nor>> <= Bitstring, Bitstring_nor =/= ?SLASHR>>, %%removes all /r
    Bitlist = binary:split(Bitstring_nor, <<?SPACE>>, [global]),
    [Command|Parameters] = Bitlist,
    {Command,Parameters}.


-module(parser).
%% -include_lib("eunit/include/eunit.hlr").
%%-compile(export_all).
-export([parse/1, bitstring/0]).
-define(SLASHR,13).
-define(SPACE,32).

split_Bitlist(Bitlist_without_r) ->
    split_Bitlist(Bitlist_without_r, [], []).
split_Bitlist([], Acc, Word) ->
    lists:reverse([Word|Acc]);
split_Bitlist([Head|Tail], Acc, Word) ->
    if	
	Head =:= ?SPACE -> NewWord = lists:reverse(Word),
	    split_Bitlist([Tail], [NewWord|Acc], []);
	Head =/= ?SPACE -> split_Bitlist([Tail], Acc, [Head|Word]);
	true -> ok
    end.



parse(Bitstring) ->
    Bitlist = bitstring_to_list(Bitstring),
    Bitlist_without_r = [X || X <- Bitlist, X =/= ?SLASHR],
    io:fwrite(Bitlist_without_r), %%Testing the output.
    Split_Bitlist = [X || [X] <- Bitlist_without_r, X =/= ?SPACE],
  %%  Split_Bitlist = split_Bitlist(Bitlist_without_r),
    io:fwrite(Split_Bitlist), %%Testing the output.
    ok.
    
bitstring() ->
    Bin1 = <<72,32,69,32,76,76,32,79>>,
    List = binary_to_list(Bin1),
    io:fwrite(List),
    ok.

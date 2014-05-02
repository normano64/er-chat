-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

user(Server, Socket)->
    Nick = database:get_nick(Socket),
    if
         Nick =/= [] ->
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_WELCOME, Nick, <<" :Welcome to the Internet Relay Chat Network ">>, Nick, <<"\r\n">>]);
        true ->
            []
    end.

nick(Nick, Server, Socket)->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    database:insert_user(Socket, Nick, Server),
                    gen_tcp:send(Socket, [<<":">>, Server, <<" NICK ">>, Nick, <<"\r\n">>]);
                _ ->
                    database:update_nick(Socket, Nick),
                    gen_tcp:send(Socket, [<<":">>, Server, <<" NICK ">>, Nick, <<"\r\n">>])
            end;
        _  ->
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?ERR_NICKNAMEINUSE, <<" ">>, Nick, <<" :Nickname is already in use.\r\n">>])
    end.
            
ping(_Pinger, Server, Socket)->
    tcp_gen:send(Socket, [<<":">>, Server, <<" PONG ">>, Server, <<" :">>, Server, <<"\r\n">>]).

%% mode(, Socket)->
%%     .

%% join(, Socket)->
%%     .

%% privmsg(, Socket)->
%%     .

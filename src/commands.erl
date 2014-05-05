-module(commands).
-compile(export_all).
-include("rpl_macro.hrl").

user(Server, Socket)->
    {_,Nick} = database:get_nick(Socket),
    if
        Nick =/= [] ->
            io:format("true~n"),
            Hostname = inet:gethostname(),
            Port = inet:port(Socket),
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_WELCOME, <<" ">>, Nick, <<" :Welcome to the Internet Relay Network ">>, Nick, <<"\r\n">>]),
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_YOURHOST, <<" ">>, Nick, <<" :Your host is localhost[">>, Hostname, <<"/">>, Port, <<"], running version er-chat-alpha-01\r\n">>]);
        true ->
            io:format("fail~n"),
            []
    end.

nick(Nick, Server, Socket)->
    case database:check_nick(Nick) of
        {_,[]} ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    database:insert_user(Socket, Nick, Server),
                    gen_tcp:send(Socket, [<<":">>, Nick, <<"!ost@localhost NICK :">>, Nick, <<"\r\n">>]);
                _ ->
                    database:update_nick(Socket, Nick),
                    gen_tcp:send(Socket, [<<":">>, Nick, <<"!ost@localhost NICK :">>, Nick, <<"\r\n">>])
            end;
        _  ->
            case database:check_socket(Socket) of
                {_,[]} ->
                    database:insert_user(Socket, <<"">>, Server);
                _ ->
                    []
            end,
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?ERR_NICKNAMEINUSE, <<" * ">>, Nick, <<" :Nickname is already in use.\r\n">>])
    end.
            
ping(_Pinger, Server, Socket)->
    gen_tcp:send(Socket, [<<":">>, Server, <<" PONG ">>, Server, <<" :">>, Server, <<"\r\n">>]).

%% mode(, Socket)->
%%     .

%% join(, Socket)->
%%     .

%% privmsg(, Socket)->
%%     .

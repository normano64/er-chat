-module(commands).
-compile(export_all).

user(Server, Socket)->
    Nick = database:get_nick(Socket),
    gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?RPL_WELCOME, Nick, <<" :Welcome to the Internet Relay Chat Network ">>, Nick, <<"\r\n">>]).
nick(Nick, Server, Socket)->
    case database:query_user(Nick) of
        {_,[]} ->
            database:insert_user(Nick, Socket, Server),
            gen_tcp:send(Socket, [<<":">>, Server, <<" NICK ">>, Nick, <<"\r\n">>]);
        _  ->
            gen_tcp:send(Socket, [<<":">>, Server, <<" ">>, ?ERR_NICKNAMEINUSE, <<" ">>, Nick, <<" :Nickname is already in use.\r\n">>])
    end
            
%% mode(, Socket)->
%%     .
ping(Server, Socket)->
    tcp_gen:send(Socket, [<<":">>, Server, <<" PONG ">>, Server, <<" :">>, Server, <<"\r\n">>]).
%% join(, Socket)->
%%     .
%% privmsg(, Socket)->
%%     .

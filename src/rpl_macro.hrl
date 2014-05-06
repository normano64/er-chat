% Connection replies 001-009
-define(RPL_WELCOME, <<"001">>). % Welcome to the Internet Relay Network <nick>!<user>@<host>
-define(RPL_YOURHOST, <<"002">>). % Your host is <servername>, running version <ver>
-define(RPL_CREATED, <<"003">>). % This server was created <date>
-define(RPL_MYINFO, <<"004">>). % <servername> <version> <available user modes> <available channel modes>
-define(RPL_BOUNCE, <<"005">>). % Try server <server name>, port <port number>

% Command replies 200-399
-define(RPL_AWAY, <<"301">>). % <nick> :<away message>

-define(RPL_NOTOPIC, <<"331">>). % <channel> :No topic is set
-define(RPL_TOPIC, <<"332">>). % <channel> :<topic>

-define(RPL_NAMREPLY, <<"353">>). % ( "=" / "*" / "@" ) <channel> :[ "@" / "+" ] <nick> *( " " [ "@" / "+" ] <nick> )

% Error replies 400-599
-define(ERR_NOSUCHNICK, <<"401">>). % <nickname> :No such nick/channel
-define(ERR_NOSUCHSERVER, <<"402">>). % <server name> :No such server"

-define(ERR_NICKNAMEINUSE, <<"433">>). % <nick> :Nickname is already in use

-define(ERR_ALREADYREGISTRED, <<"462">>). % <nick> :Nickname is already in use


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                       %
%                                                                                       %
%                                        REPLIES                                        %
%                                                                                       %
%                                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(REPLY_WELCOME,[<<":">>, Server, <<" ">>, ?RPL_WELCOME, <<" ">>, Nick, <<" :Welcome to the Internet Relay Network ">>, Nick, <<"\r\n">>]).
-define(REPLY_YOURHOST,[<<":">>, Server, <<" ">>, ?RPL_YOURHOST, <<" ">>, Nick, <<" :Your host is localhost[">>, list_to_binary(Hostname), <<"/">>, list_to_binary(integer_to_list(Port)), <<"], running version er-chat-alpha-01\r\n">>]).
-define(REPLY_ALREADYREGISTERD,[<<":">>, Server, <<" ">>, ?ERR_ALREADYREGISTRED, <<" ">>, Nick, <<" :You may not reregister\r\n">>]).

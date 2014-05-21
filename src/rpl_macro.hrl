-define(USER_PREFIX,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" ">>]).
-define(SERVER_PREFIX,[<<":">>,ServerHostent,<<" ">>]).
% Connection replies 001-009
-define(RPL_WELCOME,<<"001">>). % Welcome to the Internet Relay Network <nick>!<user>@<host>
-define(RPL_YOURHOST,<<"002">>). % Your host is <servername>,running version <ver>
-define(RPL_CREATED,<<"003">>). % This server was created <date>
-define(RPL_MYINFO,<<"004">>). % <servername> <version> <available user modes> <available channel modes>
-define(RPL_BOUNCE,<<"005">>). % Try server <server name>,port <port number>

% Command replies 200-399
-define(RPL_AWAY,<<"301">>). % <nick> :<away message>
-define(RPL_LIST,<<"322">>).
-define(RPL_LISTENED,<<"323">>).
%% Replies for whois
-define(RPL_WHOISUSER,<<"311">>).
-define(RPL_WHOISSERVER,<<"312">>).
-define(RPL_WHOISACTUALLY,<<"338">>).
-define(RPL_WHOISIDLE,<<"317">>).
-define(RPL_ENDOFWHOIS,<<"318">>).

%% Topic replies
-define(RPL_NOTOPIC,<<"331">>). % <channel> :No topic is set
-define(RPL_TOPIC,<<"332">>). % <channel> :<topic>

%% Channel replies
-define(RPL_NAMREPLY,<<"353">>). % ( "=" / "*" / "@" ) <channel> :[ "@" / "+" ] <nick> *( " " [ "@" / "+" ] <nick> )
-define(RPL_ENDOFNAMES,<<"366">>). %<channel> :End of NAMES list

% Error replies 400-599
-define(ERR_NOSUCHNICK,<<"401">>). % <nickname> :No such nick/channel
-define(ERR_NOSUCHSERVER,<<"402">>). % <server name> :No such server
-define(ERR_NOSUCHCHANNEL,<<"403">>).
-define(ERR_UNKNOWNCOMMAND,<<"421">>). %% <command> :Unknown command
-define(ERR_ERRONEUSNICKNAME,<<"432">>). %% <nick> :Erroneous nickname
-define(ERR_NICKNAMEINUSE,<<"433">>). % <nick> :Nickname is already in use
-define(ERR_NOTONCHANNEL,<<"442">>). %% <channel> :You're not on that channel
-define(ERR_ALREADYREGISTRED,<<"462">>). % <nick> :Nickname is already in use
-define(ERR_CHANOPRIVSNEEDED,<<"482">>). %% <channel> :You're not channel operator
-define(ERR_USERONCHANNEL,<<"443">>).
-define(RPL_INVITING,<<"341">>).
-define(RPL_INVITED,<<"345">>).
-define(ERR_USERNOTINCHANNEL,<<"441">>).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                                     %%
%%                                                                                     %%
%%                                        REPLIES                                      %%
%%                                                                                     %%
%%                                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(REPLY_WELCOME,[<<":">>,ServerHostent,<<" ">>,?RPL_WELCOME,<<" ">>,Nick,<<" :Welcome to the Internet Relay Network ">>,Nick,<<"\r\n">>]).
-define(REPLY_YOURHOST,[<<":">>,ServerHostent,<<" ">>,?RPL_YOURHOST,<<" ">>,Nick,<<" :Your host is ">>,ServerHostent,<<"[">>,ServerIP,<<"/">>,list_to_binary(integer_to_list(Port)),<<"],running version er-chat-alpha-01\r\n">>]).
-define(REPLY_ALREADYREGISTERD,[<<":">>,ServerHostent,<<" ">>,?ERR_ALREADYREGISTRED,<<" ">>,Nick,<<" :You may not reregister\r\n">>]).
-define(REPLY_UPDATENICK,[<<":">>,OldNick,<<"!">>,User,<<"@">>,Hostent,<<" NICK :">>,Nick,<<"\r\n">>]).
-define(REPLY_NICKNAMEINUSE,[<<":">>,ServerHostent,<<" ">>,?ERR_NICKNAMEINUSE,<<" * ">>,Nick,<<" :Nickname is already in use.\r\n">>]).
-define(REPLY_ERRONEUSNICKNAME,[?SERVER_PREFIX,<<" ">>,?ERR_ERRONEUSNICKNAME,<<" ">>,OldNick,<<" ">>,Nick,<<" :Erroneous nickname\r\n">>]).

-define(REPLY_PING,[<<"PING :">>,ServerHostent,<<"\r\n">>]).
-define(REPLY_QUIT,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" QUIT :">>,"Gone to buy cheese.",<<"\r\n">>]).
-define(REPLY_PONG,[<<"PONG ">>,ServerHostent,<<" ">>,Nick,<<"\r\n">>]).
-define(REPLY_UNKNOWNCOMMAND,[<<":">>,ServerHostent,<<" ">>,?ERR_UNKNOWNCOMMAND,<<" ">>,Command,<<" :Unknown command\r\n">>]).
-define(REPLY_JOINCHANNEL,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" JOIN :">>,Channel,<<"\r\n">>]).
-define(REPLY_JOINTOPIC,[<<":">>,ServerHostent,<<" ">>,?RPL_TOPIC,<<" ">>,Nick,<<" ">>,Channel,<<" ">>,<<":">>,Topic,<<"\r\n">>]).
-define(REPLY_JOINNOTOPIC,[<<":">>,ServerHostent,<<" ">>,?RPL_NOTOPIC,<<" ">>,Nick,<<" ">>,Channel,<<" :No topic is set\r\n">>]).
-define(REPLY_JOINNAMREPLY,[<<":">>,ServerHostent,<<" ">>,?RPL_NAMREPLY,<<" ">>,Nick,<<" = ">>,Channel,<<" :">>,UserList,<<"\r\n">>]).
-define(REPLY_ENDOFNAMES,[<<":">>,ServerHostent,<<" ">>,?RPL_ENDOFNAMES,<<" ">>,Nick,<<" ">>,Channel,<<" :End of NAMES list\r\n">>]).
-define(REPLY_PRIVMSG,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" PRIVMSG ">>,Target,<<" :">>,Message,<<"\r\n">>]).
-define(REPLY_PART,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" PART ">>,Target,<<" :">>,Message,<<"\r\n">>]).
-define(REPLY_NOSUCHCHANNEL,[?SERVER_PREFIX,<<" ">>,?ERR_NOSUCHCHANNEL,<<" ">>,?ERR_NOSUCHCHANNEL,<<" ">>,Nick,<<" ">>,Channel,<<" :No such channel\r\n">>]).

-define(REPLY_NOSUCHNICK,[<<":">>,ServerHostent,<<" ">>,?ERR_NOSUCHNICK,<<" ">>,Nick,<<" ">>,Target, <<" :No such nick/channel\r\n">>]). 
-define(REPLY_ENDOFWHOIS,[<<":">>,ServerHostent,<<" ">>,?RPL_ENDOFWHOIS,<<" ">>,Nick,<<" ">>, Target, <<" :End of /WHOIS list\r\n">>]). 
-define(REPLY_WHOISUSER,[<<":">>,ServerHostent,<<" ">>,?RPL_WHOISUSER,<<" ">>,Nick,<<" ">>,Target,<<" ">> ,UserHostent,<<" * :">>,TargetRealName,<<"\r\n">>]).
-define(REPLY_WHOISSERVER,[<<":">>,ServerHostent,<<" ">>,?RPL_WHOISSERVER,<<" ">>,Nick,<<" ">>,Target,<<" ">>,UserServer,<<" :hardcoded server name\r\n">>]).
-define(REPLY_NOTINCHANNEL,[<<":">>,ServerHostent,<<" ">>,?ERR_NOTONCHANNEL,<<" ">>,Nick,<<" ">>,Channel,<<" :You're not on that channel\r\n">>]).
-define(REPLY_NOTCHANOP,[<<":">>,ServerHostent,<<" ">>,?ERR_CHANOPRIVSNEEDED,<<" ">>,Nick,<<" ">>,Channel,<<" :You're not channel operator\r\n">>]).
-define(REPLY_NEWTOPIC,[<<":">>,Nick,<<"!">>,User,<<"@">>,Hostent,<<" TOPIC ">>,Channel,<<" :">>,Topic,<<"\r\n">>]).

%%for invite
-define(REPLY_INVITING,[?USER_PREFIX,<<"INVITE ">>, Target,<<" ">>, Channel,<<"\r\n">>]).

%%-define(REPLY_INVITED,[<<":">>,ServerHostent,<<" ">>,?RPL_INVITING,<<" ">>, <<" INVITE ">>, Target,<<" ">>, Channel]).
-define(REPLY_USERONCHANNEL,[<<":">>,ServerHostent,<<" ">>,?ERR_USERONCHANNEL,<<" ">>, Target,<<" ">>, Channel, <<" :">>,Target,Reason,<<"\r\n">>]). 

-define(REPLY_KICK_NOCOMMENT,[?USER_PREFIX,<<"KICK ">>,TargetChannel,<<" ">>,Target,<<"\r\n">>]).
-define(REPLY_KICK_COMMENT,[?USER_PREFIX,<<"KICK ">>,TargetChannel,<<" ">>,Target,<<" :">>,Comment,<<"\r\n">>]).
-define(REPLY_USERNOTONTHATCHANNEL,[?SERVER_PREFIX,?ERR_USERNOTINCHANNEL,<<" ">>,Target,<<" ">>,Channel,<<"\r\n">>]).
-define(REPLY_NOTONCHANNEL,[?SERVER_PREFIX,?ERR_NOTONCHANNEL,<<" ">>,Channel,<<"\r\n">>]).
-define(REPLY_LIST,[?SERVER_PREFIX,?RPL_LIST,<<" ">>,Channel,<<" :">>,Topic,<<"\r\n">>]).
-define(REPLY_LISTENED,[?SERVER_PREFIX,?RPL_LISTENED,<<" :">>,Info,<<"\r\n">>]).
%% :efnet.portlane.se 311 jajaja jajaja ~mattiasli nl119-199-61.student.uu.se * :realname
%% :efnet.portlane.se 312 jajaja jajaja efnet.portlane.se :Portlane EFnet Server (IPv4, IPv6 & SSL)
%% :efnet.portlane.se 338 jajaja jajaja 130.243.199.61 :actually using host
%% :efnet.portlane.se 317 jajaja jajaja 12 1399632873 :seconds idle, signon time
%% :efnet.portlane.se 318 jajaja jajaja :End of /WHOIS list.

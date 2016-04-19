-module(ujson_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).
-behavior(gen_server).

%% public behavior
-export([ start_link/1, stop/1, send/4 ]).

%% gen_server behavior
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

% service identifier
-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))).

% server record
-record(ujson_client, { port, socket, schema }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Behavior

%% start a client
start_link(Port) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, #ujson_client{ 
			port = Port,
			schema = [] }, []).

%% Stop a client
stop(Port) ->
	gen_server:call(?SELF, stop).

%% Send a message to a serverr
send(Port,Host,Dest,Message) ->
	gen_server:call(?SELF, { send, Host, Dest, Message }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Behavior

init(Client = #ujson_client{ port = Port }) ->
	{ ok, Socket } = gen_udp:open(Port, [ binary, { active, true }]),
	{ ok, Client#ujson_client{ socket = Socket } }.

handle_call({send, Host, Port, Message}, _From, Client = #ujson_client{
		socket = Socket }) ->
	gen_udp:send(Socket,Host,Port,Message),
	{ reply, ok , Client };

handle_call(stop, _From, Client) ->
	{ stop, ok, Client };

handle_call(Message, _From, Client) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, Client }.

handle_cast(Message, Client) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Client }.

handle_info( {udp, _Socket, _IP, _InPortNo, Packet}, Client) ->
	io:format("Got ~p from server~n", [ Packet ]),
	{ noreply, Client }.

terminate( _Reason, _Client) ->
	ok.

code_change( _Old, Client, _Extra ) ->
	{ ok, Client }.


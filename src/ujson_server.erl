-module(ujson_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).
-behavior(gen_server).

%% public api
-export([ start_link/3, stop/1, send/4 ]).

%% gen_server behavior
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

% service identifier
-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Src))).

% server record
-record(ujson_server, { port, module, function, socket, schema }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Behavior

%% start a server
start_link(Src,Module,Function) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, #ujson_server{ 
			port = Src,
			module = Module,
			function = Function
	}, []).

%% Send
send(Src,Addr,Port,Message) ->
	gen_server:call(?SELF, { send, Addr, Port, Message }).

%% Stop a server
stop(Src) ->
	gen_server:call(?SELF, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Behavior

init(Server = #ujson_server{ port = Src }) ->
	{ ok, Socket } = gen_udp:open(Src, [ binary, { active, true }]),
	{ ok, Server#ujson_server{ socket = Socket } }.

handle_call({ send, Addr, Port, Message }, _From, Server = #ujson_server{
	socket = Socket }) ->
	gen_udp:send(Socket,Addr,Port,Message),	
	{ reply, ok, Server };
handle_call(stop, _From, Server) ->
	{ stop, ok, Server };
handle_call(Message, _From, Server) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, Server }.

handle_cast(Message, Server) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Server }.

handle_info( {udp, _Socket, _IP, _InPortNo, Packet}, Server = #ujson_server{
	module = Module, function = Function }) ->
	spawn(Module,Function,[ Packet ]),
	{ noreply, Server }.

terminate( _Reason, _Server) ->
	ok.

code_change( _Old, Server, _Extra ) ->
	{ ok, Server }.

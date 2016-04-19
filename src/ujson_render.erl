-module(ujson_render).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ render/1 ]).

-record(ujson_request, { schema, port, ipv4, ipv6 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

render(Data) ->
	render(Data,<<>>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

render(<<>>,Acc) ->
	Acc;

render([],Acc) ->
	Acc;

render([H|T],Acc) when is_tuple(H) ->
	Acc;

render([H|T],Acc) when is_list(H) ->
	Acc;

render([H|T],Acc) when is_binary(H) ->	
	Acc;

render([H|T],Acc) when is_integer(H) ->
	Acc;

render([H|T],Acc) when is_float(H) ->
	Acc;

render(Str,Acc) when is_binary(Str) ->
	Acc;

render(Int,Acc) when is_integer(Int) ->
	Acc;

render(Float,Acc) when is_float(Float) ->
	Acc;

render(Atom,Acc) when is_atom(Atom) ->
	Acc;

render(Data,Acc) ->
	Acc.

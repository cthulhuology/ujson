-module(ujson).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ render/1, parse/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

render(false) ->
	<< $f >>;
render(true) ->
	<< $t >>;
render(null) ->
	<< $n >>;
render(<<>>) ->
	<< $s, 0:16/big-unsigned-integer >>;
render([]) ->
	<< $a, 0:16/big-unsigned-integer >>;
render([{}]) ->
	<< $o, 0:16/big-unsigned-integer >>;
render(Atom) when is_atom(Atom) ->
	render(list_to_binary(atom_to_list(Atom)));
render(String) when is_binary(String) ->	
	L = size(String),
	<< $s, L:16/big-unsigned-integer, String/binary >>;
render(Int) when is_integer(Int), Int >= -128, Int < 128 ->
	<< $c, Int:8 >>;
render(Int) when is_integer(Int), Int >= 0, Int < 256 ->
	<< $C, Int:8 >>;
render(Int) when is_integer(Int), Int >= -32768, Int < 32768 ->
	<< $w, Int:16/big-integer >>;
render(Int) when is_integer(Int), Int >= 0, Int < 65536 ->
	<< $W, Int:16/big-unsigned-integer >>;
render(Int) when is_integer(Int), Int >= -2147483648, Int < 2147483648 ->
	<< $i, Int:32/big-integer >>;
render(Int) when is_integer(Int), Int >= 0, Int < 4294967296 ->
	<< $I, Int:32/big-unsigned-integer >>;
render(Int) when is_integer(Int), Int < 0 ->
	<< $q, Int:64/big-integer >>;
render(Int) when is_integer(Int) ->
	<< $Q, Int:64/big-unsigned-integer >>;
render(Float) when is_float(Float), Float > 1.0e8 ; Float < -1.0e8 ->
	<< $D, Float:64/big-float >>;
render(Float) when is_float(Float) ->
	<< $d, Float:32/big-float >>;
render({K,V}) ->
	Len = size(K),
	Value = render(V),
	<< Len:16/big-unsigned-integer, K/binary, Value/binary >>;
render([H|T]) when is_tuple(H) ->
	render(object, [H|T], <<>>);
render([H|T]) ->
	render(array, [H|T], <<>>).

parse(<<>>) ->
	null;
parse(<< $n, Rest/binary>>) ->
	{ null, Rest };
parse(<< $t, Rest/binary>>) ->
	{ true, Rest }; 
parse(<< $f, Rest/binary>>) ->
	{ false, Rest }; 
parse(<< $c, Int:8, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $C, Int:8, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $w, Int:16/big-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $W, Int:16/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $i, Int:32/big-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $I, Int:32/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $q, Int:64/big-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $Q, Int:64/big-unsigned-integer, Rest/binary>>) ->
	{ Int, Rest };
parse(<< $d, Float:32/big-float, Rest/binary>>) ->
	{ Float, Rest }; 
parse(<< $D, Float:64/big-float, Rest/binary>>) ->
	{ Float, Rest }; 
parse(<< $s, 0:16/big-unsigned-integer, Rest/binary>>) ->
	{ <<>>, Rest };
parse(<< $s, Size:16/big-unsigned-integer, String:Size/binary, Rest/binary >>) ->
	{ String, Rest };
parse(<< $a, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ [], Rest };
parse(<< $a, Size:16/big-unsigned-integer, Array:Size/binary, Rest/binary >>) ->
	{ parse_array(Array,[]), Rest };
parse(<< $o, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ [{}], Rest };
parse(<< $o, Size:16/big-unsigned-integer, Object:Size/binary, Rest/binary >>) ->
	{ parse_object(Object,[]), Rest }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

%% render a list
render(object,[],Acc) ->
	L = size(Acc),
	<< $o, L:16/big-unsigned-integer, Acc/binary >>;
render(array,[],Acc) ->
	L = size(Acc),
	<< $a, L:16/big-unsigned-integer, Acc/binary >>;
render(Type, [H|T],Acc) ->
	Term = render(H),
	render(Type, T, << Acc/binary, Term/binary >>).

%% parse an array
parse_array(<<>>, Acc) ->
	lists:reverse(Acc);
parse_array(Array, Acc ) ->
	{ Value, Rem } = parse(Array),
	parse_array(Rem, [ Value | Acc ]).

%% parse and object
parse_object(<<>>, Acc) ->
	lists:reverse(Acc);
parse_object(<<Len:16/big-unsigned-integer, Key:Len/binary, Data/binary>>,Acc) ->
	{ Value, Rem } = parse(Data),
	parse_object(Rem, [ { Key, Value } | Acc ]).

-module(schema).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ extract/2, strip/1, stripped/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

stripped(false) ->
	<< $f >>;
stripped(true) ->
	<< $t >>;
stripped(null) ->
	<< $n >>;
stripped(<<>>) ->
	<< 0:16/big-unsigned-integer >>;
stripped([]) ->
	<< 0:16/big-unsigned-integer >>;
stripped([{}]) ->
	<< 0:16/big-unsigned-integer >>;
stripped(Atom) when is_atom(Atom) ->
	stripped(list_to_binary(atom_to_list(Atom)));
stripped(String) when is_binary(String) ->	
	L = size(String),
	<< L:16/big-unsigned-integer, String/binary >>;
stripped(Int) when is_integer(Int), Int >= -128, Int < 128 ->
	<< Int:8 >>;
stripped(Int) when is_integer(Int), Int >= 0, Int < 256 ->
	<< Int:8 >>;
stripped(Int) when is_integer(Int), Int >= -32768, Int < 32768 ->
	<< Int:16/big-integer >>;
stripped(Int) when is_integer(Int), Int >= 0, Int < 65536 ->
	<< Int:16/big-unsigned-integer >>;
stripped(Int) when is_integer(Int), Int >= -2147483648, Int < 2147483648 ->
	<< Int:32/big-integer >>;
stripped(Int) when is_integer(Int), Int >= 0, Int < 4294967296 ->
	<< Int:32/big-unsigned-integer >>;
stripped(Int) when is_integer(Int), Int < 0 ->
	<< Int:64/big-integer >>;
stripped(Int) when is_integer(Int) ->
	<< Int:64/big-unsigned-integer >>;
stripped(Float) when is_float(Float), Float > 1.0e8 ; Float < -1.0e8 ->
	<< Float:64/big-float >>;
stripped(Float) when is_float(Float) ->
	<< Float:32/big-float >>;
stripped({K,V}) ->
	Len = size(K),
	Value = stripped(V),
	<< Len:16/big-unsigned-integer, K/binary, Value/binary >>;
stripped([H|T]) when is_tuple(H) ->
	stripped(object, [H|T], <<>>);
stripped([H|T]) ->
	stripped(array, [H|T], <<>>).

strip(<<>>) ->
	null;
strip(<< $n, Rest/binary>>) ->
	{ $n, Rest };
strip(<< $t, Rest/binary>>) ->
	{ $b, Rest }; 
strip(<< $f, Rest/binary>>) ->
	{ $b, Rest }; 
strip(<< $c, _Int:8, Rest/binary>>) ->
	{ $c, Rest };
strip(<< $C, _Int:8, Rest/binary>>) ->
	{ $C, Rest };
strip(<< $w, _Int:16/big-integer, Rest/binary>>) ->
	{ $w, Rest };
strip(<< $W, _Int:16/big-unsigned-integer, Rest/binary>>) ->
	{ $W, Rest };
strip(<< $i, _Int:32/big-integer, Rest/binary>>) ->
	{ $i, Rest };
strip(<< $I, _Int:32/big-unsigned-integer, Rest/binary>>) ->
	{ $I, Rest };
strip(<< $q, _Int:64/big-integer, Rest/binary>>) ->
	{ $q, Rest };
strip(<< $Q, _Int:64/big-unsigned-integer, Rest/binary>>) ->
	{ $Q, Rest };
strip(<< $d, _Float:32/big-float, Rest/binary>>) ->
	{ $d, Rest }; 
strip(<< $D, _Float:64/big-float, Rest/binary>>) ->
	{ $D, Rest }; 
strip(<< $s, 0:16/big-unsigned-integer, Rest/binary>>) ->
	{ $s, Rest };
strip(<< $s, Size:16/big-unsigned-integer, _String:Size/binary, Rest/binary >>) ->
	{ $s, Rest };
strip(<< $a, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ $a, Rest };
strip(<< $a, Size:16/big-unsigned-integer, Array:Size/binary, Rest/binary >>) ->
	{ strip_array(Array,[]), Rest };
strip(<< $o, 0:16/big-unsigned-integer, Rest/binary >>) ->
	{ $o, Rest };
strip(<< $o, Size:16/big-unsigned-integer, Object:Size/binary, Rest/binary >>) ->
	{ strip_object(Object,[]), Rest }.

extract(Data,Schema) ->
	extract(Data,Schema,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

%% stripped a list
stripped(object,[],Acc) ->
	L = size(Acc),
	<< L:16/big-unsigned-integer, Acc/binary >>;
stripped(array,[],Acc) ->
	L = size(Acc),
	<< L:16/big-unsigned-integer, Acc/binary >>;
stripped(Type, [H|T],Acc) ->
	Term = stripped(H),
	stripped(Type, T, << Acc/binary, Term/binary >>).

%% parse an array
strip_array(<<>>, Acc) ->
	[ $a | lists:reverse(Acc) ];
strip_array(Array, Acc ) ->
	{ Value, Rem } = strip(Array),
	strip_array(Rem, [ Value | Acc ]).

%% parse and object
strip_object(<<>>, Acc) ->
	[ $o | lists:reverse(Acc) ];
strip_object(<<Len:16/big-unsigned-integer, _Key:Len/binary, Data/binary>>,Acc) ->
	{ Value, Rem } = strip(Data),
	strip_object(Rem, [ Value| Acc ]).

extract(Data,[],Acc) ->
	{ lists:reverse(Acc), Data, [] };
extract(<<>>,Schema,Acc) ->
	{ lists:reverse(Acc), <<>>, Schema };
extract(<<$n, Rest/binary>>, [ _Tag | T ], Acc ) ->
	extract(Rest,T, [ null | Acc ]);  
extract(<<$t, Rest/binary>>, [ $b | T ], Acc ) ->
	extract(Rest,T, [ true | Acc ]);  
extract(<<$f, Rest/binary>>, [ $b | T ], Acc ) ->
	extract(Rest,T, [ false | Acc ]);
extract(<<Int:8, Rest/binary>>, [ $c | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:8/signed, Rest/binary>>, [ $C | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:16/big-integer, Rest/binary>>, [ $w | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:16/big-unsigned-integer, Rest/binary>>, [ $W | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:32/big-integer, Rest/binary>>, [ $i | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:32/big-unsigned-integer, Rest/binary>>, [ $I | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:64/big-integer, Rest/binary>>, [ $q | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Int:64/big-unsigned-integer, Rest/binary>>, [ $Q | T ], Acc ) ->
	extract(Rest, T, [ Int | Acc ]);
extract(<<Float:32/big-float, Rest/binary>>, [ $d | T ], Acc ) ->
	extract(Rest, T, [ Float | Acc ]);
extract(<<Float:64/big-float, Rest/binary>>, [ $D | T ], Acc ) ->
	extract(Rest, T, [ Float | Acc ]);
extract(<<Size:16/big-unsigned-integer,String:Size/binary,Rest/binary>>, [ $s | T ], Acc ) ->
	extract(Rest, T, [ String | Acc ]);
extract(<<Size:16/big-unsigned-integer,Array:Size/binary,Rest/binary>>, [ $a | T ], Acc ) ->
	extract(Array, T, [] );
extract(<<Size:16/big-unsigned-integer,Array:Size/binary,Rest/binary>>, [ [$a|S] | T ], Acc ) ->
	extract(Rest, T, [ extract(Array,S,[]) | Acc ]);

extract(<<Size:16/big-unsigned-integer,Object:Size/binary,Rest/binary>>, [ $o | T ], Acc ) ->
	extract_object(Object, T, [] );
extract(<<Size:16/big-unsigned-integer,Object:Size/binary,Rest/binary>>, [ [$o|S] | T ], Acc ) ->
	extract(Rest, T, [ extract_object(Object,S,[]) | Acc ]).

extract_object(<<>>, _, Acc ) ->
	lists:reverse(Acc);
extract_object(_, [], Acc ) ->
	lists:reverse(Acc);
extract_object(<<Len:16/big-unsigned-integer,Key:Len/binary,Value/binary>>, [ Tag | T ], Acc) ->
	{ [ V ], Rem, _ } = extract(Value, [ Tag ], []),
	io:format("extracted ~p~n", [ V ]),	
	extract_object(Rem, T, [ { Key, V } | Acc ]).
	

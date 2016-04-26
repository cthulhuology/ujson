-module(ujson_parser).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ parse/1, parse/2 ]).

-record(ujson_request, { schema, port, ipv4, ipv6 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

parse(Data) when is_binary(Data) ->
	parse(Data,[]).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%
%%

%% parse
%%
%% +--------+------~-+
%% | schema | data.. |
%% +--------+------~-+
%%
%% Extract the schema index 0-255, and then dispatch parsing according to schema
%%
parse(<<$/,Bytes/binary>>, _Acc) ->
	io:format("Parsing ~p~n", [ Bytes ]),
	parse_request(Bytes);			% ujson request
parse(<< 0, Bytes/binary>>, Acc) ->
	parse(Bytes,"U",Acc);
parse(<<Schema:8,Bytes/binary>>, Acc) -> 
	Format = ujson_schema:lookup(Schema),
	parse(Bytes,Format,Acc).

parse(<<>>,[],Acc) ->
	lists:reverse(Acc);
parse(<<>>,_Format,_Acc) ->
	io:format("incomplete message~n");
parse(<<Bytes/binary>>,[], Acc ) ->
	io:format("extra data ~p~n", [ Bytes ]),
	lists:reverse(Acc);
parse(<<Bytes/binary>>, [ Op | Format], Acc) ->
	case Op of
		$b -> parse_bool(Bytes,Format,Acc);	% boolean
		$C -> parse_u8(Bytes,Format,Acc);	% unsigned byte
		$c -> parse_s8(Bytes,Format,Acc);	% signed byte
		$W -> parse_u16(Bytes,Format,Acc);	% unsigned short
		$w -> parse_s16(Bytes,Format,Acc);	% signed short
		$I -> parse_u32(Bytes,Format,Acc);	% unsigned int
		$i -> parse_s32(Bytes,Format,Acc);	% signed int
		$Q -> parse_u64(Bytes,Format,Acc);	% unsigned long long
		$q -> parse_s64(Bytes,Format,Acc);	% unsigned long long
		$f -> parse_f32(Bytes,Format,Acc);	% float
		$d -> parse_f64(Bytes,Format,Acc);	% double
		$s -> parse_str(Bytes,Format,Acc);	% string
		$A -> parse_static_array(Bytes,Format,Acc);	% static array
		$a -> parse_array(Bytes,Format,Acc);	% dynamic array
		$O -> parse_static_object(Bytes,Format,Acc);	% static object
		$o -> parse_object(Bytes,Format,Acc);	% dynamic object
		$U -> parse_schema(Bytes,Format,Acc)	% ujson inline schema
	end.

%% tag b
%%
%% +--------+
%% | Byte   |
%% +--------+
parse_bool(<<Bool:8/integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { bool, Bool /= 0 } | Acc ]).

%% tag C
%%
%% +--------+
%% | Byte   |
%% +--------+
parse_u8(<<Int:8/integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { u8, Int } | Acc ]).

%% tag c
%%
%% +--------+
%% | byte   |
%% +--------+
parse_s8(<<Int:8/signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { s8, Int } | Acc ]).

%% tag W
%% +----------------+
%% | Word           |
%% +----------------+
parse_u16(<<Int:16/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { u16, Int } | Acc ]).

%% tag w
%% +----------------+
%% | signed word    |
%% +----------------+
parse_s16(<<Int:16/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { s16, Int } | Acc ]).

%% tag I
%% +--------------------------------+
%% | Integer                        |
%% +--------------------------------+
parse_u32(<<Int:32/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { u32, Int } | Acc ]).

%% tag i
%% +--------------------------------+
%% | signed integer                 |
%% +--------------------------------+
parse_s32(<<Int:32/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { s32, Int } | Acc ]).

%% tag Q
%% +----------------------------------------------------------------+
%% | long integer                                                   |
%% +----------------------------------------------------------------+
parse_u64(<<Int:64/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { u64, Int } | Acc ]).

%% tag q
%% +----------------------------------------------------------------+
%% | long signed integer                                            |
%% +----------------------------------------------------------------+
parse_s64(<<Int:64/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { s64, Int } | Acc ]).

%% tag f
%% +--------------------------------+
%% | float                          |
%% +--------------------------------+
parse_f32(<<Float:32/big-float,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { f32, Float } | Acc ]).

%% tag d
%% +----------------------------------------------------------------+
%% | double                                                         |
%% +----------------------------------------------------------------+
parse_f64(<<Float:64/big-float,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { f64, Float } | Acc ]).

%% tag s
%% +----------------+------~-+
%% | size           | string |
%% +----------------+------~-+
parse_str(<<Count:16/big-integer,Str:Count/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { string, Str } | Acc ]).


%% tag A
%%
%% Static Array Parsing
%%  +--------+--------+--------+
%%  | schema | size   | ...    |
%%  +--------+--------+--------+
parse_static_array(<<Schema:8,Size:16/big-integer,Array:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { array, parse(Schema,Array,[]) } | Acc ]).

%% tag a
%%
%% Dynamic Array Parsing
%%
%%  +----------------+-------+------~-+--------+------~-+-----~--+
%%  | size           | tag   | value  | tag    | value  | ...    |
%%  +----------------+-------+------~-+--------+------~-+-----~--+
parse_array(<<Size:16/big-integer,Array:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { object, parse_array(Array,[]) } | Acc ]).

parse_array(<<>>, Acc) ->
	lists:reverse(Acc);
parse_array(<<$b,Bool:8/integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { bool, Bool /= 0 } | Acc ]);
parse_array(<<$C,Int:8/integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { u8, Int } | Acc ]);
parse_array(<<$c,Int:8/integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { s8, Int } | Acc ]);
parse_array(<<$W,Int:16/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { u16, Int } | Acc ]);
parse_array(<<$w,Int:16/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { s16, Int } | Acc ]);
parse_array(<<$I,Int:32/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { u32, Int } | Acc ]);
parse_array(<<$i,Int:32/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { s32, Int } | Acc ]);
parse_array(<<$Q,Int:64/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { u64, Int } | Acc ]);
parse_array(<<$q,Int:64/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { s64, Int } | Acc ]);
parse_array(<<$f,Float:32/float,Values/binary>>, Acc) ->
	parse_array(Values, [ { f32, Float } | Acc ]);
parse_array(<<$d,Float:64/float,Values/binary>>, Acc) ->
	parse_array(Values, [ { f64, Float } | Acc ]);
parse_array(<<$s,Count:16/big-integer,Str:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { string, Str } | Acc ]);
parse_array(<<$a,Count:16/big-integer,Array:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { array, parse_array(Array,[]) } | Acc ]);
parse_array(<<$o,Count:16/big-integer,Object:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { object, parse_object(Object,[]) } | Acc ]); 	
parse_array(<<_,_Values/binary>>, _Acc) ->
	io:format("invalid tag in array~n").

%% tag O
%%
%% Static Object Parsing
%%
%%  +--------+--------+--------+
%%  | schema | size   | ...    |
%%  +--------+--------+--------+
parse_static_object(<<Schema:8,Size:16/big-integer,Object:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { object, parse(Schema,Object,[]) } | Acc ]).

%% tag o
%%
%% Dynamic Object Parsing
%%
%%  +----------------+-------+-------+------~-+--------+--------+------~-+-----~--+
%%  | size           | key   | tag   | value  | key    | tag    | value  | ...    |
%%  +----------------+-------+-------+------~-+--------+--------+------~-+-----~--+
parse_object(<<Size:16/big-integer,Object:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { array, parse_object(Object,[]) } | Acc ]).

parse_object(<<>>, Acc) ->
	lists:reverse(Acc);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$b,Bool:8/integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, bool, Bool /= 0 } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$C,Int:8/integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, u8, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$c,Int:8/integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, s8, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$W,Int:16/big-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, u16, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$w,Int:16/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, s16, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$I,Int:32/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { Key, u32, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$i,Int:32/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, s32, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$Q,Int:64/big-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, u64, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$q,Int:64/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, s64, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$f,Float:32/float,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, f32, Float } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$d,Float:64/float,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, f64, Float } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$s,Count:16/big-integer,Str:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, string, Str } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$a,Count:16/big-integer,Array:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, array, parse_array(Array,[]) } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$o,Count:16/big-integer,Object:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, object, parse_object(Object,[]) } | Acc ]); 	
parse_object(<<_Keylen:16/big-integer,_Key:_Keylen/binary,_,_Values/binary>>, _Acc) ->
	io:format("invalid tag in object~n").

%% tag U
%%
%% Inline Schema parsing
%%  
%%  +--------+-----------------+------~-+------~-+
%%  | $U     | size            | tags.. | data.. |
%%  +--------+-----------------+------~-+------~-+
parse_schema(<<$U,Size:16/big-integer, Tags:Size/binary,Bytes/binary>>,_Format,Acc) ->
	parse(Bytes,binary:bin_to_list(Tags),Acc).

%% tag /
%%
%% Schema request
%%  
%%  +---------+----------------+---------------~-+
%%  | Schema  | port           | ipv4/ipv6       |
%%  +---------+----------------+---------------~-+
parse_request(<<Schema:8/integer,Port:16/big-integer,IPv6:128/big-integer>>) ->
	#ujson_request{ schema = Schema, port = Port, ipv6 = IPv6 };
parse_request(<<Schema:8/integer,Port:16/big-integer,IPv4:32/big-integer>>) ->
	#ujson_request{ schema = Schema, port = Port, ipv4 = IPv4 }.

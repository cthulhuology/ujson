-module(ujson_parser).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J. Goehrig"/utf8>>).

-export([ parse/1 ]).

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
parse(<<Schema:8,Bytes/binary>>, Acc) -> 
	Format = schema:lookup(Schema),
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
		$B -> parse_u8(Bytes,Format,Acc);	% unsigned byte
		$b -> parse_s8(Bytes,Format,Acc);	% signed byte
		$W -> parse_u16(Bytes,Format,Acc);	% unsigned short
		$w -> parse_s16(Bytes,Format,Acc);	% signed short
		$I -> parse_u32(Bytes,Format,Acc);	% unsigned int
		$i -> parse_s32(Bytes,Format,Acc);	% signed int
		$Q -> parse_u64(Bytes,Format,Acc);	% unsigned long long
		$q -> parse_s64(Bytes,Format,Acc);	% unsigned long long
		$f -> parse_f32(Bytes,Format,Acc);	% float
		$d -> parse_f64(Bytes,Format,Acc);	% double
		$s -> parse_str(Bytes,Format,Acc);	% string
		$A -> parse_sarr(Bytes,Format,Acc);	% schema array
		$a -> parse_arr(Bytes,Format,Acc);	% dynamic array
		$O -> parse_sobj(Bytes,Format,Acc);	% schema object
		$o -> parse_obj(Bytes,Format,Acc)	% dynamic object
	end.

%% tag B
%%
%% +--------+
%% | Byte   |
%% +--------+
parse_u8(<<Int:8/integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $B, Int } | Acc ]).

%% tag b
%%
%% +--------+
%% | byte   |
%% +--------+
parse_s8(<<Int:8/signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $b, Int } | Acc ]).

%% tag W
%% +----------------+
%% | Word           |
%% +----------------+
parse_u16(<<Int:16/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $W, Int } | Acc ]).

%% tag w
%% +----------------+
%% | signed word    |
%% +----------------+
parse_s16(<<Int:16/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $w, Int } | Acc ]).

%% tag I
%% +--------------------------------+
%% | Integer                        |
%% +--------------------------------+
parse_u32(<<Int:32/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $I, Int } | Acc ]).

%% tag i
%% +--------------------------------+
%% | signed integer                 |
%% +--------------------------------+
parse_s32(<<Int:32/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $i, Int } | Acc ]).

%% tag Q
%% +----------------------------------------------------------------+
%% | long integer                                                   |
%% +----------------------------------------------------------------+
parse_u64(<<Int:64/big-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $Q, Int } | Acc ]).

%% tag q
%% +----------------------------------------------------------------+
%% | long signed integer                                            |
%% +----------------------------------------------------------------+
parse_s64(<<Int:64/big-signed-integer,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $q, Int } | Acc ]).

%% tag f
%% +--------------------------------+
%% | float                          |
%% +--------------------------------+
parse_f32(<<Float:32/big-float,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $f, Float } | Acc ]).

%% tag d
%% +----------------------------------------------------------------+
%% | double                                                         |
%% +----------------------------------------------------------------+
parse_f64(<<Float:64/big-float,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $d, Float } | Acc ]).

%% tag s
%% +----------------+------~-+
%% | size           | string |
%% +----------------+------~-+
parse_str(<<Count:16/big-integer,Str:Count/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $s, Str } | Acc ]).


%% tag A
%%
%% Static Array Parsing
%%  +--------+--------+--------+
%%  | schema | size   | ...    |
%%  +--------+--------+--------+
parse_sarr(<<Schema:8,Size:16/big-integer,Array:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $A, parse(Schema,Array,[]) } | Acc ]).

%% tag a
%%
%% Dynamic Array Parsing
%%
%%  +----------------+-------+------~-+--------+------~-+-----~--+
%%  | size           | tag   | value  | tag    | value  | ...    |
%%  +----------------+-------+------~-+--------+------~-+-----~--+
parse_arr(<<Size:16/big-integer,Array:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $a, parse_array(Array,[]) } | Acc ]).

parse_array(<<>>, Acc) ->
	lists:reverse(Acc);
parse_array(<<$B,Int:8/integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $B, Int } | Acc ]);
parse_array(<<$b,Int:8/integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $b, Int } | Acc ]);
parse_array(<<$W,Int:16/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $W, Int } | Acc ]);
parse_array(<<$w,Int:16/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $w, Int } | Acc ]);
parse_array(<<$I,Int:32/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $I, Int } | Acc ]);
parse_array(<<$i,Int:32/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $i, Int } | Acc ]);
parse_array(<<$Q,Int:64/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $Q, Int } | Acc ]);
parse_array(<<$q,Int:64/big-signed-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { $q, Int } | Acc ]);
parse_array(<<$f,Float:32/float,Values/binary>>, Acc) ->
	parse_array(Values, [ { $f, Float } | Acc ]);
parse_array(<<$d,Float:64/float,Values/binary>>, Acc) ->
	parse_array(Values, [ { $d, Float } | Acc ]);
parse_array(<<$s,Count:16/big-integer,Str:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { $s, Str } | Acc ]);
parse_array(<<$a,Count:16/big-integer,Array:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { $a, parse_array(Array,[]) } | Acc ]);
parse_array(<<$o,Count:16/big-integer,Object:Count/binary,Values/binary>>, Acc) ->
	parse_array(Values, [ { $o, parse_object(Object,[]) } | Acc ]); 	
parse_array(<<_,_Values/binary>>, _Acc) ->
	io:format("invalid tag in array~n").

%% tag O
%%
%% Static Object Parsing
%%
%%  +--------+--------+--------+
%%  | schema | size   | ...    |
%%  +--------+--------+--------+
parse_sobj(<<Schema:8,Size:16/big-integer,Object:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $O, parse(Schema,Object,[]) } | Acc ]).

%% tag o
%%
%% Dynamic Object Parsing
%%
%%  +----------------+-------+-------+------~-+--------+--------+------~-+-----~--+
%%  | size           | key   | tag   | value  | key    | tag    | value  | ...    |
%%  +----------------+-------+-------+------~-+--------+--------+------~-+-----~--+
parse_obj(<<Size:16/big-integer,Object:Size/binary,Bytes/binary>>, Format, Acc) ->
	parse(Bytes, Format, [ { $a, parse_object(Object,[]) } | Acc ]).

parse_object(<<>>, Acc) ->
	lists:reverse(Acc);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$B,Int:8/integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $B, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$b,Int:8/integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $b, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$W,Int:16/big-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $W, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$w,Int:16/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $w, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$I,Int:32/big-integer,Values/binary>>, Acc) ->
	parse_array(Values, [ { Key, $I, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$i,Int:32/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $i, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$Q,Int:64/big-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $Q, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$q,Int:64/big-signed-integer,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $q, Int } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$f,Float:32/float,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $f, Float } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$d,Float:64/float,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $d, Float } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$s,Count:16/big-integer,Str:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $s, Str } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$a,Count:16/big-integer,Array:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $a, parse_array(Array,[]) } | Acc ]);
parse_object(<<Keylen:16/big-integer,Key:Keylen/binary,$o,Count:16/big-integer,Object:Count/binary,Values/binary>>, Acc) ->
	parse_object(Values, [ { Key, $o, parse_object(Object,[]) } | Acc ]); 	
parse_object(<<_Keylen:16/big-integer,_Key:_Keylen/binary,_,_Values/binary>>, _Acc) ->
	io:format("invalid tag in object~n").



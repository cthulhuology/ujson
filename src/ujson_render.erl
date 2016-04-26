-module(ujson_render).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ render/1, render/2, format/2 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

render(true) ->
	<< -1:8 >>;

render(false) ->
	<< 0:8 >>;

render(null) ->
	<< 0:16/big-unsigned-integer >>;

render(<<>>) ->
	<< 0:16/big-unsigned-integer >>;

render([]) ->
	<< 0:16/big-unsigned-integer >>;

render([{}]) ->
	<< 0:16/big-unsigned-integer >>;

render(Atom) when is_atom(Atom) ->
	render(list_to_binary(atom_to_list(Atom)));

render(String) when is_binary(String) ->	
	L = size(String),
	<< L:16/big-unsigned-integer, String/binary >>;

render(Int) when is_integer(Int) ->
	<< Int:32/big-integer >>;

render(Float) when is_float(Float) ->
	<< Float:64/big-float >>;

render({K,V}) ->
	Tag = tag(V),
	Key = render(K),
	Value = render(V),
	<< Key/binary, Tag/binary, Value/binary >>;

render([H|T]) ->
	render([H|T],<<>>).

format(Data,Format) ->
	format(Data,Format,<<>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

%% render a list
render([],Acc) ->
	L = size(Acc),
	<< L:16/big-unsigned-integer, Acc/binary>>;
render([{K,V}|T],Acc) ->
	Term = render({K,V}),
	render( T, << Acc/binary, Term/binary>>);
render([H|T],Acc) ->
	Tag = tag(H),
	Term = render(H),
	render( T, << Acc/binary, Tag/binary, Term/binary>>).

%% format
%% 

format([D|DT],[H|T],Acc) ->
	case H of
		$b -> format(DT,T,<< Acc, D:8 >>);
		$C -> format(DT,T,<< Acc, D:8 >>);
		$c -> format(DT,T,<< Acc, D:8 >>);
		$W -> format(DT,T,<< Acc, D:16/big-unsigned-integer >>);
		$w -> format(DT,T,<< Acc, D:16/big-integer >>);
		$I -> format(DT,T,<< Acc, D:32/big-unsigned-integer >>);
		$i -> format(DT,T,<< Acc, D:32/big-integer >>);
		$Q -> format(DT,T,<< Acc, D:64/big-unsigned-integer >>);
		$q -> format(DT,T,<< Acc, D:64/big-integer >>);
		$f -> format(DT,T,<< Acc, D:32/float >>);
		$d -> format(DT,T,<< Acc, D:64/float >>);
		$s -> format_string(DT,T,D,Acc);
		$A -> format_static_array(DT,T,D,Acc);
		$a -> format_array(DT,T,D,Acc);
		$0 -> format_static_object(DT,T,D,Acc);
		$o -> format_object(DT,T,D,Acc);
		$U -> format_schema(DT,T,D,Acc)
	end.	

format_string(DT,T,null,Acc) ->
	format( DT, T, << Acc/binary, 0:16/big-unsigned-integer >>);
format_string(DT,T,D,Acc) when is_binary(D) ->
	Len = size(D),
	format( DT, T, << Acc/binary, Len:16/big-unsigned-integer, D/binary>>).

format_array(DT,T,null,Acc) ->
	format( DT, T, << Acc, 0:16/big-unsigned-integer >>);
format_array(DT,T,D,Acc) when is_list(D) ->
	Bin = format_tagged(D,<<>>),
	Len = size(Bin),
	format(	DT, T, << Acc/binary, Len:16/big-unsigned-integer, Bin/binary>>).

format_tagged([],Acc) ->
	Acc;
format_tagged([H|T],Acc) ->
	<< C:8 >> = Tag = tag(H),
	Val = format( [H], [C], <<>>),
	format_tagged( T, << Acc/binary, Tag/binary, Val/binary >>).

format_object(DT,T,null,Acc) ->
	format( DT, T, << Acc/binary, 0:16/big-unsigned-integer >>);
format_object(DT,T,D,Acc) ->
	Bin = format_tagged_kv(D,<<>>),
	Len = size(Bin),
	format( DT, T, << Acc/binary, Len:16/big-unsigned-integer, Bin/binary>>).

format_tagged_kv([],Acc) ->
	Acc;
format_tagged_kv([{K,V}|T],Acc) ->
	SLen = size(K),
	<< C:8>> = Tag = tag(V),
	Val = format( [V], [C], <<>>),
	format_tagged_kv( T, << Acc/binary, SLen:16/big-unsigned-integer,
		K/binary, Tag/binary, Val/binary >>).

format_static_array(DT,[S|T],null,Acc) ->
	format( DT, T, << Acc/binary, S:8, 0:16/big-unsigned-integer >>);
format_static_array(DT,[S|T],A,Acc) ->
	Schema = ujson_schema:lookup(S),
	Bin = format( A, Schema, <<>>),
	Len = size(Bin),
	format( DT, T, << Acc/binary, S:8, Len:16/big-unsigned-integer,
		Bin/binary >>).

format_static_object(DT,[S|T],null,Acc) ->
	format( DT, T, << Acc/binary, S:8, 0:16/big-unsigned-integer >>);
format_static_object(DT,[S|T],O,Acc) ->
	Schema = ujson_schema:lookup(S),
	Bin = format( O, Schema, <<>>),
	Len = size(Bin),
	format( DT, T, << Acc/binary, S:8, Len:16/big-unsigned-integer,
		Bin/binary >>). 
	
format_schema(DT,T,S,Acc) ->
	Len = size(S),
	format( DT, T, << Acc/binary, $U, Len:16/big-unsigned-integer,
		S/binary >>).

%% tags for dynamic types
%%
tag(true) -> 
	<< $b >>;
tag(false) ->
	<< $b >>;
tag(null) ->
	<< $o >>;
tag(Term) when is_integer(Term) ->
	<< $i >>;
tag(Term) when is_float(Term) ->
	<< $d >>;
tag(Term) when is_binary(Term) ->
	<< $s >>;
tag([H|_T]) when is_tuple(H) ->
	<< $o >>;
tag(Term) when is_list(Term) ->
	<< $a >>.


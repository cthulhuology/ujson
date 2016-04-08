-module(ujson_tojson).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J. Goehrig"/utf8>>).

-export([ tojson/1 ]).

tojson(List) ->
	tojson(List,<<>>).

tojson([], Acc) ->
	Acc;
tojson([ {$b, Int} | Terms ], Acc) ->
	X = binary:list_to_bin(integer_to_list(Int)),
	tojson(Terms,<<Acc,X/binary>>);
tojson([ {$B, Int} | Terms ], Acc) ->
	X = binary:list_to_bin(integer_to_list(Int)),
	tojson(Terms,<<Acc,X/binary>>);


-module(ujson_echo).

-export([ echo/1 ]).

echo(Message) ->
	io:format("~p~n", [ Message ]).

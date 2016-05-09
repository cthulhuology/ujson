-module(ujson_echo).

-export([ echo/1 ]).

echo(Message) ->
	Data = ujson:decode(Message),
	io:format("~p~n", [ Data ]).

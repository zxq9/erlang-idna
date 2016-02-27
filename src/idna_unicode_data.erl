-module(idna_unicode_data).
-compile([compressed]).

-export([lookup/1]).
-export([decomposition/1]).

-include("idna_unicode_data.hrl").

lookup("") -> false;
lookup(Codepoint) ->
	case lists:keyfind(Codepoint, 1, ?UNICODE_DATA) of
		{_, A, B, C} -> {A, B, C};
		false -> false
	end.

decomposition("") -> false;
decomposition(Key) ->
	case lists:keyfind(Key, 3, ?UNICODE_DATA) of
		{A, _, _, _} -> A;
		false -> false
	end.
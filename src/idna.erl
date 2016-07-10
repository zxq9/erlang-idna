-module(idna).

-export([to_ascii/1,
         to_unicode/1]).

-define(ACE_PREFIX, "xn--").

to_ascii(Domain) when is_binary(Domain) ->
    Domain1 = unicode:characters_to_list(stringprep:nameprep(Domain)),
    to_ascii1(Domain1);
to_ascii(Domain) when is_list(Domain) ->
  to_ascii(iolist_to_binary(Domain)).

to_ascii1(Domain) ->
  Parts = string:tokens(Domain, "."),
  to_ascii(Parts, []).

-spec to_unicode(nonempty_string()) -> binary().
to_unicode(Domain) ->
  unicode:characters_to_binary(from_ascii(Domain), utf8).

from_ascii(Domain) ->
  from_ascii(string:tokens(Domain, "."), []).


%% Helper functions
%%
to_ascii([], Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
    case lists:all(fun(C) -> is_ascii(C) end, Label) of
        true ->
            Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(Label)
    end.

from_ascii([], Acc) ->
    lists:reverse(Acc);
from_ascii([Label|Labels], []) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label)));
from_ascii([Label|Labels], Acc) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label), [$.|Acc])).

label_from_ascii(?ACE_PREFIX ++ Label) ->
    punycode:decode(Label);
label_from_ascii(Label) ->
    Label.


is_ascii(Ch) when is_integer(Ch), Ch >= 0, Ch =< 127 -> true;
is_ascii(_) -> false.
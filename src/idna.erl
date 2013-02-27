-module(idna).

-export([start/0, to_ascii/1, utf8_to_ascii/1]).

-define(ACE_PREFIX, "xn--").

start() ->
  idna_unicode_data:start(),
  idna_unicode_data:load("http://www.unicode.org/Public/UNIDATA/UnicodeData.txt").

to_ascii(Domain) ->
  to_ascii(string:tokens(idna_unicode:downcase(Domain), "."), []).

utf8_to_ascii(Domain) ->
  to_ascii(xmerl_ucs:from_utf8(Domain)).

to_ascii([], Acc) ->
  lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
  to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
  to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
  case lists:all(fun(C) -> xmerl_ucs:is_ascii(C) end, Label) of
    true ->
      Label;
    false ->
      ?ACE_PREFIX ++ punycode:encode(idna_unicode:normalize_kc(Label))
  end.

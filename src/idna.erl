-module(idna).

-export([to_ascii/1,
         utf8_to_ascii/1,
         from_ascii/1]).

-define(ACE_PREFIX, "xn--").

to_ascii(Domain) -> to_ascii(Domain, []).

to_ascii(Domain, Options) ->
    validate_options(Options),
    Parts = case proplists:get_value(strict, Options, false) of
               false ->
                 re:split(lowercase(Domain), "[.。．｡]", [{return, list}, unicode]);
              true ->
                string:tokens(lowercase(Domain), ".")
            end,

    to_ascii(Parts, Options, []).

utf8_to_ascii(Domain) -> utf8_to_ascii(Domain, []).

utf8_to_ascii(Domain, Options) ->
    to_ascii(idna_ucs:from_utf8(Domain), Options).

-spec from_ascii(nonempty_string()) -> nonempty_string().
from_ascii(Domain) ->
    from_ascii(string:tokens(Domain, "."), []).

%% Helper functions
%%

validate_options([]) -> ok;
validate_options([strict|Rs]) -> validate_options(Rs);
validate_options([{strict, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([std3_rules|Rs]) -> validate_options(Rs);
validate_options([{std3_rules, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([transitional|Rs]) -> validate_options(Rs);
validate_options([{transitional, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([_]) -> erlang:error(badarg).

to_ascii([], _Options, Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], Options, []) ->
    to_ascii(Labels, Options, lists:reverse(label_to_ascii(Label, Options)));
to_ascii([Label|Labels], Options, Acc) ->
    to_ascii(Labels, Options, lists:reverse(label_to_ascii(Label, Options), [$.|Acc])).

label_to_ascii(Label, Options) ->
    case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label) of
        true ->
            Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(characters_to_nfkc_list(uts46_remap(Label, Options)))
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


%% Lowercase all chars in Str
-spec lowercase(String::unicode:chardata()) -> unicode:chardata().
lowercase(CD) when is_list(CD) ->
  lowercase_list(CD);
lowercase(CD) when is_binary(CD) ->
  lowercase_bin(CD,<<>>).

lowercase_list(CPs0) ->
  case unicode_util_compat:lowercase(CPs0) of
    [Char|CPs] -> append(Char,lowercase_list(CPs));
    [] -> []
  end.

lowercase_bin(CPs0, Acc) ->
  case unicode_util_compat:lowercase(CPs0) of
    [Char|CPs] when is_integer(Char) ->
      lowercase_bin(CPs, <<Acc/binary, Char/utf8>>);
    [Chars|CPs] ->
      lowercase_bin(CPs, <<Acc/binary,
                           << <<CP/utf8>> || CP <- Chars>>/binary >>);
    [] -> Acc
  end.

append(Char, <<>>) when is_integer(Char) -> [Char];
append(Char, <<>>) when is_list(Char) -> Char;
append(Char, Bin) when is_binary(Bin) -> [Char,Bin];
append(Char, Str) when is_integer(Char) -> [Char|Str];
append(GC, Str) when is_list(GC) -> GC ++ Str.


characters_to_nfkc_list(CD) ->
    case unicode_util_compat:nfc(CD) of
        [CPs|Str] when is_list(CPs) -> CPs ++ characters_to_nfkc_list(Str);
        [CP|Str] -> [CP|characters_to_nfkc_list(Str)];
        [] -> []
    end.


uts46_remap(Str, Options) ->
  Std3Rules = proplists:get_value(std3_rules, Options, true),
  Transitional = proplists:get_value(transitional, Options, false),
  uts46_remap(Str, Std3Rules, Transitional).

uts46_remap([Cp|Rs], Std3Rules, Transitional) ->
  {Status, Replacement, _IStatus} =  idna_mapping:uts46_map(Cp),
  if
    ((Status =:= valid) orelse
     (Status =:= deviation andalso Transitional =:= false) orelse
     (Status =:= disallowed_STD3_mapped andalso Std3Rules =:= false andalso Replacement =:= undefined)) ->
      [Cp] ++ uts46_remap(Rs, Std3Rules, Transitional);
    ((Replacement =/= undefined) andalso
     ((Status =:= mapped) orelse
      (Status =:= disallowed_STD3_mapped andalso Std3Rules =:= false) orelse
      (Status =:= deviation andalso Transitional =:= true))) ->
      Replacement ++ uts46_remap(Rs, Std3Rules, Transitional);
    Status =:= ignored ->
       uts46_remap(Rs, Std3Rules, Transitional);
    true ->
      erlang:exit({invalid_codepoint, Cp})
  end;
uts46_remap([], _, _) ->
  [].

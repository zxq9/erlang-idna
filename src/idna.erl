-module(idna).

%% API
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% compatibility API
-export([to_ascii/1,
         utf8_to_ascii/1,
         from_ascii/1]).


-export([check_hyphen/1, check_nfc/1, check_context/1, check_initial_combiner/1]).

-export([check_label/1]).

-define(ACE_PREFIX, "xn--").

encode(Domain) ->
  encode(Domain, []).

encode(Domain0, Options) ->
  ok = validate_options(Options),
  Domain = case proplists:get_value(uts46, Options, false) of
             true ->
               STD3Rules = proplists:get_value(std3_rules, Options, false),
               Transitional = proplists:get_value(transitional, Options, false),
               uts46_remap(Domain0, STD3Rules, Transitional);
             false -> Domain0
           end,
  Labels = case proplists:get_value(strict, Options, false) of
             false ->
               re:split(lowercase(Domain), "[.。．｡]", [{return, list}, unicode]);
             true ->
               string:tokens(lowercase(Domain), ".")
           end,

  case Labels of
    [] -> exit(empty_domain);
    _ ->
      encode_1(Labels, [])
  end.

decode(Domain) ->
  decode(Domain, []).

decode(Domain0, Options) ->
  ok = validate_options(Options),
  Domain = case proplists:get_value(uts46, Options, false) of
             true ->
               STD3Rules = proplists:get_value(std3_rules, Options, false),
               uts46_remap(Domain0, STD3Rules, false);
             false ->
               Domain0
           end,
  Labels = case proplists:get_value(strict, Options, false) of
             false ->
               re:split(lowercase(Domain), "[.。．｡]", [{return, list}, unicode]);
             true ->
               string:tokens(lowercase(Domain), ".")
           end,

  case Labels of
    [] -> exit(empty_domain);
    _ ->
      decode_1(Labels, [])
  end.


%% Compatibility API
%%

to_ascii(Domain) -> encode(Domain, [uts46]).


utf8_to_ascii(Domain) ->
  to_ascii(idna_ucs:from_utf8(Domain)).

-spec from_ascii(nonempty_string()) -> nonempty_string().
from_ascii(Domain) ->
  decode(Domain).


%% Helper functions
%%

validate_options([]) -> ok;
validate_options([uts46|Rs]) -> validate_options(Rs);
validate_options([{uts46, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([strict|Rs]) -> validate_options(Rs);
validate_options([{strict, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([std3_rules|Rs]) -> validate_options(Rs);
validate_options([{std3_rules, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([transitional|Rs]) -> validate_options(Rs);
validate_options([{transitional, B}|Rs]) when is_boolean(B) -> validate_options(Rs);
validate_options([_]) -> erlang:error(badarg).

encode_1([], Acc) ->
  lists:reverse(Acc);
encode_1([Label|Labels], []) ->
  encode_1(Labels, lists:reverse(alabel(Label)));
encode_1([Label|Labels], Acc) ->
  encode_1(Labels, lists:reverse(alabel(Label), [$.|Acc])).

check_nfc(Label) ->
  case characters_to_nfc_list(Label) of
    Label -> ok;
    _ ->
      erlang:exit({bad_label, {nfc, "Label must be in Normalization Form C"}})
  end.

check_hyphen([_, _, $-, $-|_]) ->
  erlang:exit({bad_label, {hyphen, "Label has disallowed hyphens in 3rd and 4th position"}});
check_hyphen([$- | _]) ->
  erlang:exit({bad_label, {hyphen, "Label must not start with an hyphen"}});
check_hyphen(Label) ->
  case lists:last(Label) of
    "-" ->
      erlang:exit({bad_label, {hyphen, "Label must not end with an hyphen"}});
    _ ->
      ok
  end.

check_initial_combiner([CP|_]) ->
  case idna_data:lookup(CP) of
    {[$M|_], _} ->
      erlang:exit({bad_label, {initial_combiner, "Label begins with an illegal combining character"}});
    _ ->
      ok
  end.

check_context(Label) -> check_context(Label, Label, 1).

check_context([CP | Rest], Label, Pos) ->
  case idna_table:lookup(CP) of
    'PVALID' ->
      check_context(Rest, Label, Pos+1);
    'CONTEXTJ' ->
      case idna_context:valid_contextj(CP, Label, Pos) of
        true ->
          check_context(Rest, Label, Pos+1);
        false ->
          ErrorMsg = io:format("Joiner ~p pnot allowed at posion ~p in ~p", [CP, Pos, Label]),
          erlang:exit({bad_label, {contextj, ErrorMsg}})

      end;
    'CONTEXTO' ->
      case idna_context:valid_contexto(CP, Label, Pos) of
        true ->
          check_context(Rest, Label, Pos+1);
        false ->
          ErrorMsg = io:format("Joiner ~p pnot allowed at posion ~p in ~p", [CP, Pos, Label]),
          erlang:exit({bad_label, {contextj, ErrorMsg}})
      end;
    _ ->
      ErrorMsg = io:format("Codepoint ~p pnot allowed at posion ~p in ~p", [CP, Pos, Label]),
      erlang:exit({bad_label, {contextj, ErrorMsg}})
  end;
check_context([], _, _) ->
  ok.

check_label(Label) ->
  ok = check_nfc(Label),
  ok = check_hyphen(Label),
  ok = check_initial_combiner(Label),
  ok = check_context(Label),
  ok = idna_bidi:check_bidi(Label),
  ok.

alabel(Label) ->
  ok = check_label(Label),
  case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label) of
    true ->
      Label;
    false ->
      ?ACE_PREFIX ++ punycode:encode(Label)
  end.

decode_1([], Acc) ->
  lists:reverse(Acc);
decode_1([Label|Labels], []) ->
  decode_1(Labels, lists:reverse(ulabel(Label)));
decode_1([Label|Labels], Acc) ->
  decode_1(Labels, lists:reverse(ulabel(Label), [$.|Acc])).

ulabel(?ACE_PREFIX ++ Label0) ->
  Label = punycode:decode(Label0),
  ok = check_label(Label),
  Label;
ulabel(Label) ->
  ok = check_label(Label),
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


characters_to_nfc_list(CD) ->
  case unicode_util_compat:nfc(CD) of
    [CPs|Str] when is_list(CPs) -> CPs ++ characters_to_nfc_list(Str);
    [CP|Str] -> [CP|characters_to_nfc_list(Str)];
    [] -> []
  end.

uts46_remap(Str, Std3Rules, Transitional) ->
  characters_to_nfc_list(uts46_remap_1(Str, Std3Rules, Transitional)).

uts46_remap_1([Cp|Rs], Std3Rules, Transitional) ->
  {Status, Replacement, _IStatus} =  idna_mapping:uts46_map(Cp),
  if
    ((Status =:= valid) orelse
     (Status =:= deviation andalso Transitional =:= false) orelse
     (Status =:= disallowed_STD3_mapped andalso Std3Rules =:= false andalso Replacement =:= undefined)) ->
      [Cp] ++ uts46_remap_1(Rs, Std3Rules, Transitional);
    ((Replacement =/= undefined) andalso
     ((Status =:= mapped) orelse
      (Status =:= disallowed_STD3_mapped andalso Std3Rules =:= false) orelse
      (Status =:= deviation andalso Transitional =:= true))) ->
      Replacement ++ uts46_remap_1(Rs, Std3Rules, Transitional);
    Status =:= ignored ->
      uts46_remap_1(Rs, Std3Rules, Transitional);
    true ->
      erlang:exit({invalid_codepoint, Cp})
  end;
uts46_remap_1([], _, _) ->
  [].

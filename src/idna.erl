%% -*- coding: utf-8 -*-
-module(idna).

%% API
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% compatibility API
-export([to_ascii/1,
         to_unicode/1,
         utf8_to_ascii/1,
         from_ascii/1]).


-export([alabel/1, ulabel/1]).

-export([check_hyphen/1, check_nfc/1, check_context/1, check_initial_combiner/1, check_label_length/1]).

-export([check_label/1]).

-define(ACE_PREFIX, "xn--").

-ifdef('OTP_RELEASE').
-define(lower(C), string:lowercase(C)).
-else.
-define(lower(C), string:to_lower(C)).
-endif.



encode(Domain) ->
  encode(Domain, []).

encode(Domain0, Options) ->
  ok = validate_options(Options),
  Domain = case proplists:get_value(uts46, Options, false) of
             true ->
               STD3Rules = proplists:get_value(std3_rules, Options, false),
               Transitional = proplists:get_value(transitional, Options, false),
               uts46_remap(Domain0, STD3Rules, Transitional);
             false ->
               Domain0
           end,
  Labels = case proplists:get_value(strict, Options, false) of
             false ->
               re:split(Domain, "[.。．｡]", [{return, list}, unicode]);
             true ->
               string:tokens(Domain, ".")
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
               STD3Rules = proplists:get_value(std3_rules, Options, true),
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

to_ascii(Domain) -> encode(Domain).
to_unicode(Domain) -> decode(Domain).


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



check_hyphen(Label) when length(Label) >= 3 ->
  case lists:nthtail(2, Label) of
    [$-, $-|_] ->
      ErrorMsg = error_msg("Label ~p has disallowed hyphens in 3rd and 4th position", [Label]),
      erlang:exit({bad_label, {hyphen, ErrorMsg}});
    _ ->
      case (lists:nth(1, Label) == $-) orelse (lists:last(Label) == $-) of
        true ->
          erlang:exit({bad_label, {hyphen, "Label must not start or end with a hyphen"}});
        false ->
          ok
      end
  end;
check_hyphen(Label) ->
  case (lists:nth(1, Label) == $-) orelse (lists:last(Label) == $-) of
    true ->
      erlang:exit({bad_label, {hyphen, "Label must not start or end with a hyphen"}});
    false ->
      ok
  end.

check_initial_combiner([CP|_]) ->
  case idna_data:lookup(CP) of
    {[$M|_], _} ->
      erlang:exit({bad_label, {initial_combiner, "Label begins with an illegal combining character"}});
    _ ->
      ok
  end.

check_context(Label) -> check_context(Label, Label, 0).

check_context([CP | Rest], Label, Pos) ->
  case idna_table:lookup(CP) of
    'PVALID' ->
      check_context(Rest, Label, Pos + 1);
    'CONTEXTJ' ->
      case idna_context:valid_contextj(CP, Label, Pos) of
        true ->
          check_context(Rest, Label, Pos + 1);
        false ->
          ErrorMsg = error_msg("Joiner ~p not allowed at posion ~p in ~p", [CP, Pos, Label]),
          erlang:exit({bad_label, {contextj, ErrorMsg}})

      end;
    'CONTEXTO' ->
      case idna_context:valid_contexto(CP, Label, Pos) of
        true ->
          check_context(Rest, Label, Pos + 1);
        false ->
          ErrorMsg = error_msg("Joiner ~p not allowed at posion ~p in ~p", [CP, Pos, Label]),
          erlang:exit({bad_label, {contexto, ErrorMsg}})
      end;
    _Status ->
      ErrorMsg = error_msg("Codepoint ~p not allowed (~p) at posion ~p in ~p", [CP, _Status, Pos, Label]),
      erlang:exit({bad_label, {context, ErrorMsg}})
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

check_label_length(Label) when length(Label) > 63 ->
  ErrorMsg = error_msg("The label ~p  is too long", [Label]),
  erlang:exit({bad_label, {too_long, ErrorMsg}});
check_label_length(_) ->
  ok.

alabel([$x,$n,$-,$-|_]=Label0) ->
  Label1 = try ulabel(Label0)
          catch
            _:_ ->
              ErrorMsg = error_msg("The label ~p  is not a valid A-label", [Label0]),
              erlang:exit({bad_label, {alabel, ErrorMsg}})
          end,
  Label = ?ACE_PREFIX ++ punycode:encode(Label1),
  if
    Label == Label0 -> Label;
    true ->
      ErrorMsg2 = error_msg("The label ~p  s not a valid A-label", [Label0]),
      erlang:exit({bad_label, {alabel, ErrorMsg2}})
  end;
alabel(Label0) ->
  Label = case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label0) of
            true ->
              Label0;
            false ->
              ok = check_label(Label0),
              ?ACE_PREFIX ++ punycode:encode(Label0)
          end,
  ok = check_label_length(Label),
  Label.

decode_1([], Acc) ->
  lists:reverse(Acc);
decode_1([Label|Labels], []) ->
  decode_1(Labels, lists:reverse(ulabel(Label)));
decode_1([Label|Labels], Acc) ->
  decode_1(Labels, lists:reverse(ulabel(Label), [$.|Acc])).

ulabel(Label0) ->
  Label = case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label0) of
            true ->
              case Label0 of
                [$x,$n,$-,$-|Label1] ->
                  punycode:decode(lowercase(Label1));
                _ ->
                  lowercase(Label0)
              end;
            false ->
              Label0
          end,
  ok = check_label(Label),
  Label.

%ulabel([$x,$n,$-,$-|Label0]) ->
%  Label = punycode:decode(lowercase(Label0)),
%  io:format("after decode ulabel ~p~n", [Label]),
%  ok = check_label(Label),
%  Label;
%ulabel(Label) ->
%  io:format("ulabel ~p~n", [Label]),
%  ok = check_label(lowercase(Label)),
%  Label.


%% Lowercase all chars in Str
-spec lowercase(String::unicode:chardata()) -> unicode:chardata().
lowercase(CD) when is_list(CD) ->
  try lowercase_list(CD, false)
  catch unchanged -> CD
  end;
lowercase(<<CP1/utf8, Rest/binary>>=Orig) ->
  try lowercase_bin(CP1, Rest, false) of
    List -> unicode:characters_to_binary(List)
  catch unchanged -> Orig
  end;
lowercase(<<>>) ->
  <<>>.


lowercase_list([CP1|[CP2|_]=Cont], _Changed) when $A =< CP1, CP1 =< $Z, CP2 < 256 ->
  [CP1+32|lowercase_list(Cont, true)];
lowercase_list([CP1|[CP2|_]=Cont], Changed) when CP1 < 128, CP2 < 256 ->
  [CP1|lowercase_list(Cont, Changed)];
lowercase_list([], true) ->
  [];
lowercase_list([], false) ->
  throw(unchanged);
lowercase_list(CPs0, Changed) ->
  case unicode_util_compat:lowercase(CPs0) of
    [Char|CPs] when Char =:= hd(CPs0) -> [Char|lowercase_list(CPs, Changed)];
    [Char|CPs] -> append(Char,lowercase_list(CPs, true));
    [] -> lowercase_list([], Changed)
  end.

lowercase_bin(CP1, <<CP2/utf8, Bin/binary>>, _Changed)
  when $A =< CP1, CP1 =< $Z, CP2 < 256 ->
  [CP1+32|lowercase_bin(CP2, Bin, true)];
lowercase_bin(CP1, <<CP2/utf8, Bin/binary>>, Changed)
  when CP1 < 128, CP2 < 256 ->
  [CP1|lowercase_bin(CP2, Bin, Changed)];
lowercase_bin(CP1, Bin, Changed) ->
  case unicode_util_compat:lowercase([CP1|Bin]) of
    [CP1|CPs] ->
      case unicode_util_compat:cp(CPs) of
        [Next|Rest] ->
          [CP1|lowercase_bin(Next, Rest, Changed)];
        [] when Changed ->
          [CP1];
        [] ->
          throw(unchanged)
      end;
    [Char|CPs] ->
      case unicode_util_compat:cp(CPs) of
        [Next|Rest] ->
          [Char|lowercase_bin(Next, Rest, true)];
        [] ->
          [Char]
      end
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

%%uts46_remap("[$x,$n,$-,$-|_"=Str, _, _) -> Str;
uts46_remap(Str, Std3Rules, Transitional) ->
  characters_to_nfc_list(uts46_remap_1(Str, Std3Rules, Transitional)).

uts46_remap_1([Cp|Rs], Std3Rules, Transitional) ->
  Row = try idna_mapping:uts46_map(Cp)
        catch
          error:badarg  ->
            erlang:exit({invalid_codepoint, Cp})
        end,
  {Status, Replacement} = case Row of
                            {_, _} -> Row;
                            S -> {S, undefined}
                          end,
  if
    (Status =:= 'V');
    ((Status =:= 'D') andalso (Transitional =:= false));
    ((Status =:= '3') andalso (Std3Rules =:= true) andalso (Replacement =:= undefined)) ->
      [Cp] ++ uts46_remap_1(Rs, Std3Rules, Transitional);
    (Replacement =/= undefined) andalso (
        (Status =:= 'M') orelse
          (Status =:= '3' andalso Std3Rules =:= false) orelse
          (Status =:= 'D' andalso Transitional =:= true)) ->
      Replacement ++ uts46_remap_1(Rs, Std3Rules, Transitional);
    (Status =:= 'I') ->
      uts46_remap_1(Rs, Std3Rules, Transitional);
    true ->
      erlang:exit({invalid_codepoint, Cp})
  end;
uts46_remap_1([], _, _) ->
  [].

error_msg(Msg, Fmt) ->
  lists:flatten(io_lib:format(Msg, Fmt)).

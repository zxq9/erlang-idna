%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2018 21:57
%%%-------------------------------------------------------------------
-module(idna_context).
-author("benoitc").

%% API
-export([
  valid_contextj/3,
  valid_contexto/3,
  contexto_with_rule/1
]).

-define(virama_combining_class, 9).

valid_contextj(16#200c, Label, Pos) when Pos > 0 ->
      case unicode_util_compat:lookup(lists:nth(Pos -1, Label)) of
        #{ ccc := ?virama_combining_class } -> true;
        _ ->
          case range(lists:reverse(lists:sublist(Label, 1, Pos -1))) of
            false -> fales;
            true ->
              range(lists:nthtail(Label, Pos +1))
          end
      end;
valid_contextj(16#200c, Label, Pos) ->
  case range(lists:reverse(lists:sublist(Label, 1, Pos -1))) of
    false -> fales;
    true ->
      range(lists:nthtail(Label, Pos +1))
  end;
valid_contextj(16#200d, Label, Pos) when Pos > 0->
  case unicode_util_compat:lookup(lists:nth(Pos -1, Label)) of
    #{ ccc := ?virama_combining_class } -> true;
    _ -> false
  end;
valid_contextj(_, _, _) ->
  false.


range([CP|Rest]) ->
  case idna_data:joining_types(CP) of
    "T" -> range(Rest);
    "L" -> true;
    "D" -> true;
    _ ->
      range(Rest)
  end;
range([]) ->
  false.

valid_contexto(CP, Label, Pos) ->
  Len = length(Label),
  case CP of
    16#00B7 ->
      % MIDDLE DOT
      if
        (Pos > 0); (Pos < (Len -1)) -> false;
        true ->
          case lists:sublist(Label, Pos - 1, Pos +1) of
            [16#006C, _, 16#006C] -> true;
            _ -> false
          end
      end;
    16#0375 ->
      % GREEK LOWER NUMERAL SIGN (KERAIA)
      if
        (Pos < (Len -1)), Len > 1 -> false;
        true ->
          case idna_data:scripts(lists:nth(Pos +1, Label)) of
            "Greek" -> true;
            _ -> false
          end
      end;
    CP when CP == 16#05F3; CP == 16#05F4 ->
      % HEBREW PUNCTUATION GERESH or HEBREW PUNCTUATION GERSHAYIM
      if
        Pos > 0 ->
          case idna_data:scripts(lists:nth(Pos - 1, Label)) of
            "Hebrew" -> true;
            _ -> false
          end;
        true ->
          false
      end;
    CP when 16#0660 =< CP; CP =< 16#0669 ->
      % ARABIC-INDIC DIGITS
      contexto_in_range(tuple_to_list(Label), 16#0660, 16#0669);
    CP when 16#06F0 =< CP; CP =< 16#06F9 ->
      % EXTENDED ARABIC-INDIC DIGIT
      contexto_in_range(tuple_to_list(Label), 16#06F0, 16#06F9);
    CP when CP == 16#30FB ->
      % KATAKANA MIDDLE DOT
      script_ok(Label, false)
  end.


contexto_in_range([C | _], Start, End) when C >= Start, C =< End -> false;
contexto_in_range([_|Rest], Start, End) -> contexto_in_range(Rest, Start, End);
contexto_in_range([], _, _) -> true.

script_ok([C | Rest], OK) ->
  case idna_data:scripts(C) of
    "Hiragana" -> script_ok(Rest, true);
    "Katakana" -> script_ok(Rest, true);
    "Han" -> script_ok(Rest, true);
    _ ->
      script_ok(Rest, OK)
  end.

contexto_with_rule(16#00B7) -> true;
% MIDDLE DOT
contexto_with_rule(16#0375) -> true;
% GREEK LOWER NUMERAL SIGN (KERAIA)
contexto_with_rule(16#05F3) -> true;
% HEBREW PUNCTUATION GERESH
contexto_with_rule(16#05F4) -> true;
% HEBREW PUNCTUATION GERSHAYIM
contexto_with_rule(16#30FB) -> true;
% KATAKANA MIDDLE DOT
contexto_with_rule(CP) when 16#0660 =< CP, CP =< 16#0669 -> true;
% ARABIC-INDIC DIGITS
contexto_with_rule(CP) when 16#06F0 =< CP, CP =< 16#06F9 -> true;
% KATAKANA MIDDLE DOT
contexto_with_rule(_) -> false.

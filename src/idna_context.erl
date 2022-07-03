%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
-module(idna_context).
-author("benoitc").

-export([valid_contextj/2, valid_contextj/3,
         valid_contexto/2, valid_contexto/3,
         contexto_with_rule/1]).

-define(virama_combining_class, 9).

-define(fw_middle_dot, 16#30FB). % Full-width dot (Katakana)
-define(geresh,        16#05F3). % Hebrew
-define(gershayim,     16#05F4). % Hebrew
-define(keraia,        16#0375). % Greek
-define(hw_middle_dot, 16#00B7). % Half-width dot (math)
-define(ai_lo,         16#0660). % Arabic-Indic digit lower bound
-define(ai_hi,         16#0669). % Arabic-Indic digit upper bound
-define(xai_lo,        16#06F0). % Extended Arabic-Indic digit lower bound
-define(xai_hi,        16#06F9). % Extended Arabic-Indic digit upper bound



%%% API

valid_contextj([], _) ->
    true;
valid_contextj(Label, Pos) ->
    CP = lists:nth(Pos + 1, Label),
    valid_contextj(CP, Label, Pos).


valid_contextj(16#200C, Label, Pos) when Pos > 0 ->
    case unicode_util:lookup(lists:nth(Pos, Label)) of
        #{ccc := ?virama_combining_class} -> true;
        _                                 -> valid_contextj_(Label, Pos)
    end;
valid_contextj(16#200C, Label, Pos) when Pos =< 0 ->
    valid_contextj_(Label, Pos);
valid_contextj(16#200D, Label, Pos) when Pos > 0 ->
    case unicode_util:lookup(lists:nth(Pos, Label)) of
        #{ccc := ?virama_combining_class} -> true;
        _                                 -> false
    end;
valid_contextj(_, _, _) ->
  false.


valid_contextj_(Label, Pos) ->
    case range(lists:reverse(lists:nthtail(Pos, Label))) of
        true  -> range(lists:nthtail(Pos + 2, Label));
        false -> false
    end.


range([CP | Rest]) ->
    case idna_data:joining_types(CP) of
        "T" -> range(Rest);
        "L" -> true;
        "D" -> true;
        _   -> range(Rest)
    end;
range([]) ->
    false.


valid_contexto([], _) ->
    io:format("ici", []),
    true;
valid_contexto(Label, Position) ->
    CodePoint = lists:nth(Position + 1, Label),
    valid_contexto(CodePoint, Label, Position).


valid_contexto(?hw_middle_dot, Label, Pos)
        when (Pos > 0) andalso (Pos < length(Label) - 1) ->
    case lists:sublist(Label, Pos, 3) of
        [16#006C, _, 16#006C] -> true;
        _                     -> false
    end;
valid_contexto(?keraia, Label, Pos) ->
    LabelLength = length(Label),
    case (Pos < LabelLength - 1) andalso (LabelLength > 1) of
        true  -> idna_data:scripts(lists:nth(Pos + 2, Label)) =:= "greek";
        false -> false
    end;
valid_contexto(?fw_middle_dot, Label, _) ->
    script_ok(Label);
valid_contexto(?geresh, Label, Pos) when Pos > 0 ->
    idna_data:scripts(lists:nth(Pos, Label)) =:= "hebrew";
valid_contexto(?gershayim, Label, Pos) when Pos > 0 ->
    idna_data:scripts(lists:nth(Pos, Label)) =:= "hebrew";
valid_contexto(CP, Label, _) when CP >= ?ai_lo, ?ai_hi >= CP ->
    contexto_in_range(Label, ?xai_lo, ?xai_hi);
valid_contexto(CP, Label, _) when ?xai_lo =< CP, CP =< ?xai_hi ->
    contexto_in_range(Label, ?ai_lo, ?ai_hi);
valid_contexto(_, _, _) ->
    false.


contexto_in_range(CodePoints, Start, End) ->
    InBounds = fun(CP) -> CP >= Start andalso CP =< End end,
    lists:all(InBounds, CodePoints).


script_ok([?fw_middle_dot | Rest]) ->
    script_ok(Rest);
script_ok([C | Rest]) ->
    case idna_data:scripts(C) of
        "hiragana" -> true;
        "katakana" -> true;
        "han"      -> true;
        _          -> script_ok(Rest)
    end;
script_ok([]) ->
    false.


contexto_with_rule(?hw_middle_dot)                       -> true;
contexto_with_rule(?keraia)                              -> true;
contexto_with_rule(?geresh)                              -> true;
contexto_with_rule(?gershayim)                           -> true;
contexto_with_rule(?fw_middle_dot)                       -> true;
contexto_with_rule(CP) when  ?ai_lo =< CP, CP =<  ?ai_hi -> true;
contexto_with_rule(CP) when ?xai_lo =< CP, CP =< ?xai_hi -> true;
contexto_with_rule(_)                                    -> false.

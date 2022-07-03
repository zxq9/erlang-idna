%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%

-module(idna_bidi).
-author("benoitc").

-export([check_bidi/1, check_bidi/2]).



%%% API

check_bidi(Label) ->
    check_bidi(Label, false).


check_bidi(Label, CheckLtr) ->
    %% Bidi rules should only be applied if string contains RTL characters
    case CheckLtr orelse check_rtl(Label, Label) of
        false ->
            ok;
        true ->
            C = hd(Label),
            RTL = bidi_rule1(C, Label),
            check_bidi(Label, RTL, false, undefined)
    end.



%%% Implementation

check_rtl([C | Rest], Label) ->
    case idna_data:bidirectional(C) of
        false ->
            bidi_error("Unknown directionality in label=~tp c=~tw.", [Label, C]);
        Dir ->
            case lists:member(Dir, ["R", "AL", "AN"]) of
                true  -> true;
                false -> check_rtl(Rest, Label)
            end
    end;
check_rtl([], _) ->
    false.


bidi_rule1(C, Label) ->
    case idna_data:bidirectional(C) of
        "R"  -> true;
        "AL" -> true;
        "L"  -> false;
        _    ->
            Format = "First codepoint in label ~tp must be directionality L, R or AL.",
            bidi_error(Format, [Label])
    end.


check_bidi([C | Rest], true, ValidEnding, NumberType) ->
    Dir = idna_data:bidirectional(C),
    BidiRule2 = ["R", "AL", "AN", "EN", "ES", "CS", "ET", "ON", "BN", "NSM"],
    ValidEnding2 =
        case lists:member(Dir, BidiRule2) of
            true ->
                BidiRule3 =["R", "AL", "AN", "EN"],
                case lists:member(Dir, BidiRule3) of
                    true                     -> true;
                    false when Dir =/= "NSM" -> false;
                    false                    -> ValidEnding
                end;
            false ->
                bidi_error("Invalid direction for codepoint in a right-to-left label.")
        end,
    BidiRule4 = ["AN", "EN"],
    NumberType2 =
        case lists:member(Dir, BidiRule4) of
            true when NumberType =:= undefined ->
                Dir;
            true when NumberType /= Dir ->
                bidi_error("Can not mix numeral types in a right-to-left label.");
            _ ->
                NumberType
        end,
    check_bidi(Rest, true, ValidEnding2, NumberType2);
check_bidi([C | Rest], false, ValidEnding, NumberType) ->
    Dir = idna_data:bidirectional(C),
    BidiRule5 = ["L", "EN", "ES", "CS", "ET", "ON", "BN", "NSM"],
    ValidEnding2 =
        case lists:member(Dir, BidiRule5) of
            true ->
                % bidi rule 6
                BidiRule6 = ["L", "EN"],
                case lists:member(Dir, BidiRule6) of
                    true                     -> true;
                    false when Dir =/= "NSM" -> false;
                    false                    -> ValidEnding
                end;
            false ->
                bidi_error("Invalid direction for codepoint in a left-to-right label.")
        end,
    check_bidi(Rest, false, ValidEnding2, NumberType);
check_bidi([], _, false, _) ->
    bidi_error("Label ends with illegal codepoint directionality.");
check_bidi([], _, true, _) ->
    ok.


bidi_error(Format, []) ->
    bidi_error(Format);
bidi_error(Format, Args) ->
    Message = unicode:characters_to_list(io_lib:format(Format, Args)),
    bidi_error(Message).

bidi_error(Message) ->
    {error, {bidi, Message}}.

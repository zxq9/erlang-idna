%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
-module(idna).

%% API
-export([encode/1, encode/2,
         decode/1, decode/2]).

-export([check_label/1, check_label/2]).

-export_type([errors/0]).

-define(ACE_PREFIX, "xn--").


-type idna_flags() :: [Flag :: {uts46, boolean()}
                             | {std3_rules, boolean()}
                             | {transitional, boolean()}].

-type errors() :: empty_domain
                | bad_option
                | bad_label
                | too_long
                | nfc
                | hyphen
                | initial_combiner
                | {context, string()}
                | {contextj, string()}
                | {contexto, string()}
                | {invalid_codepoint, integer()}
                | {bidi, string()}
                | overflow
                | badarg.


-spec encode(String) -> Result
    when String      :: unicode:chardata(),
         Result      :: {ok, IDNA_String} | {error, Reason},
         IDNA_String :: string(),
         Reason      :: errors().
%% @doc encode Internationalized Domain Names using IDNA protocol

encode(Domain) ->
    encode(Domain, []).


-spec encode(String, Flags) -> Result
    when String      :: unicode:chardata(),
         Flags       :: idna_flags(),
         Result      :: {ok, IDNA_String} | {error, Reason},
         IDNA_String :: string(),
         Reason      :: errors().
%% @doc encode Internationalized Domain Names using IDNA protocol.
%% Input can be mapped to unicode using
%% [uts46](https://unicode.org/reports/tr46/#Introduction)
%% by setting the `uts46' flag to `true' (default is `false'). If transition from
%% IDNA 2003 to IDNA 2008 is needed, the flag `transitional' can be set to `true',
%% (default is `false'). If conformance to STD3 is needed, the flag `std3_rules'
%% can be set to `true'. (default is `false').

encode(String, Options) ->
    Validated = validate_options(Options),
    Target =
        case proplists:get_value(uts46, Validated, false) of
            true ->
                STD3Rules = proplists:get_value(std3_rules, Validated, false),
                Transitional = proplists:get_value(transitional, Validated, false),
                uts46_remap(String, STD3Rules, Transitional);
            false ->
                String
        end,
    Labels =
        case proplists:get_value(strict, Validated, false) of
            false -> re:split(Target, "[.。．｡]", [{return, list}, unicode]);
            true  -> string:tokens(Target, ".")
        end,
    encode2(Labels).


-spec decode(IDNA_String) -> Result
    when IDNA_String :: string(),
         Result      :: {ok, String} | {error, Reason},
         String      :: string(),
         Reason      :: errors().
%% @doc decode an International Domain Name encoded with the IDNA protocol

decode(IDNA_String) ->
    decode(IDNA_String, []).


-spec decode(IDNA_String, Flags) -> Result
    when IDNA_String :: string(),
         Flags       :: idna_flags(),
         Result      :: {ok, String} | {error, Reason},
         String      :: string(),
         Reason      :: errors().
%% @doc decode an International Domain Name encoded with the IDNA protocol

decode(IDNA_String, Options) ->
    Validated = validate_options(Options),
    Target =
        case proplists:get_value(uts46, Validated, false) of
            true ->
                STD3Rules = proplists:get_value(std3_rules, Validated, false),
                Transitional = proplists:get_value(transitional, Validated, false),
                uts46_remap(IDNA_String, STD3Rules, Transitional);
            false ->
                IDNA_String
        end,
    Lowered = string:lowercase(Target),
    Labels =
        case proplists:get_value(strict, Validated, false) of
            false -> re:split(Lowered, "[.。．｡]", [{return, list}, unicode]);
            true  -> string:tokens(Lowered, ".")
        end,
    decode2(Labels).


-spec check_label(string()) -> ok | {error, bad_label}.

check_label(Label) ->
    check_label(Label, [nfc, combiner, hyphens, joiners, bidi, length]).


-spec check_label(Label, Checks) -> Result
    when Label  :: string(),
         Checks :: [Check],
         Check  :: nfc | combiner | hyphens | joiners | bidi,
         Result :: ok | {error, {bad_label, Check}}.
%% @doc validate a label of  a domain

check_label(Label, Checks) ->
    do_checks(Label, lists:map(fun map_check/1, Checks)).



%%% Implementation

validate_options([Option | Rest]) when is_atom(Option) ->
    [{Option, true} | validate_options(Rest)];
validate_options([Option | Rest]) when is_tuple(Option) ->
    [Option | validate_options(Rest)];
validate_options([]) ->
    [].


alabel(Label) ->
    case lists:all(fun(C) -> is_ascii(C) end, Label) of
        true  -> alabel_ascii(Label);
        false -> alabel_puny(Label)
    end.

alabel_ascii(Label) ->
    case ulabel(Label) of
        {ok, _} ->
            case check_label_length(Label) of
                ok    -> {ok, Label};
                Error -> Error
            end;
        Error ->
            Error
    end.

alabel_puny(Label) ->
    case check_label(Label) of
        ok ->
            case punycode:encode(Label) of
                {ok, Encoded} ->
                    case check_label_length(Encoded) of
                        ok    -> {ok, ?ACE_PREFIX ++ Encoded};
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


ulabel("") ->
    {ok, ""};
ulabel(Label) ->
    case lists:all(fun(C) -> is_ascii(C) end, Label) of
        true  -> ulabel_ascii(Label);
        false -> ulabel_wild(Label)
    end.

ulabel_ascii("xn--" ++ Encoded) ->
    case punycode:decode(string:lowercase(Encoded)) of
        {ok, Decoded} ->
            case check_label(Decoded) of
                ok    -> {ok, Decoded};
                Error -> Error
            end;
        Error ->
            Error
    end;
ulabel_ascii(Label) ->
    ulabel_wild(Label).


ulabel_wild(Label) ->
    Lowered = string:lowercase(Label),
    case check_label(Lowered) of
        ok    -> {ok, Lowered};
        Error -> Error
    end.


encode2([])     -> {error, empty_domain};
encode2(Labels) -> encode3(Labels, []).


encode3([], Acc) ->
    {ok, unicode:characters_to_list(lists:reverse(Acc))};
encode3([Label | Rest], []) ->
    case alabel(Label) of
        {ok, Converted} -> encode3(Rest, lists:reverse(Converted));
        Error           -> Error
    end;
encode3([Label | Rest], Acc) ->
    case alabel(Label) of
        {ok, Converted} -> encode3(Rest, [Converted, $. | Acc]);
        Error           -> Error
    end.


decode2([])     -> {error, empty_domain};
decode2(Labels) -> decode3(Labels, []).

decode3([], Acc) ->
    {ok, unicode:characters_to_list(lists:reverse(Acc))};
decode3([Label | Rest], []) ->
    case ulabel(Label) of
        {ok, Converted} -> decode3(Rest, lists:reverse(Converted));
        Error           -> Error
    end;
decode3([Label | Rest], Acc) ->
    case ulabel(Label) of
        {ok, Converted} -> decode3(Rest, [Converted, $. | Acc]);
        Error           -> Error
    end.


do_checks(Label, [Check | Rest]) ->
    case Check(Label) of
        ok    -> do_checks(Label, Rest);
        Error -> Error
    end;
do_checks(_, []) ->
    ok.

map_check(nfc)      -> fun check_nfc/1;
map_check(combiner) -> fun check_initial_combiner/1;
map_check(hyphens)  -> fun check_hyphen/1;
map_check(joiners)  -> fun check_context/1;
map_check(bidi)     -> fun idna_bidi:check_bidi/1;
map_check(length)   -> fun check_label_length/1.


check_nfc(Label) ->
    case unicode:characters_to_nfc_list(Label) of
        Label -> ok;
        _     -> {error, nfc}
    end.


check_hyphen([$- | _]) ->
    {error, hyphen};
check_hyphen([_, _, $-, $- | _]) ->
    {error, hyphen};
check_hyphen(Label) ->
    case lists:last(Label) =:= $- of
        false -> ok;
        true  -> {error, hyphen}
    end.


check_initial_combiner([CP | _]) ->
    case idna_data:lookup(CP) of
        {[$M | _], _} ->
            {error, initial_combiner};
        _ ->
            ok
    end.


check_context(Label) ->
  check_context(Label, Label, 0).


check_context([CP | Rest], Label, Pos) ->
    case idna_table:lookup(CP) of
        'PVALID' ->
            check_context(Rest, Label, Pos + 1);
        'CONTEXTJ' ->
            case valid_contextj(CP, Label, Pos) of
                ok    -> check_context(Rest, Label, Pos + 1);
                Error -> Error
            end;
        'CONTEXTO' ->
            case valid_contexto(CP, Label, Pos) of
                ok    -> check_context(Rest, Label, Pos + 1);
                Error -> Error
            end;
        Status ->
            Format = "Codepoint ~tp not allowed (~tp) at position ~tp in ~tp",
            Message = format(Format, [CP, Status, Pos, Label]),
            {error, {context, Message}}
    end;
check_context([], _, _) ->
    ok.


valid_contextj(CP, Label, Pos) ->
    case idna_context:valid_contextj(CP, Label, Pos) of
        true ->
            ok;
        false ->
            Format = "Joiner ~tp not allowed at position ~tp in ~tp",
            Message = format(Format, [CP, Pos, Label]),
            {error, {contextj, Message}}
    end.


valid_contexto(CP, Label, Pos) ->
    case idna_context:valid_contexto(CP, Label, Pos) of
        true ->
            ok;
        false ->
            Format = "Joiner ~tp not allowed at position ~tp in ~tp",
            Message = format(Format, [CP, Pos, Label]),
            {error, {contexto, Message}}
    end.


check_label_length(Label) ->
    case length(Label) > 63 of
        false -> ok;
        true  -> {error, too_long}
    end.


uts46_remap(Str, Std3Rules, Transitional) ->
    case uts46_remap(Str, Std3Rules, Transitional, []) of
        {ok, Remapped} -> unicode:characters_to_nfc_list(Remapped);
        Error          -> Error
    end.

uts46_remap([CP | Rest], Std3Rules, Transitional, Acc) ->
    case idna_mapping:uts46_map(CP) of
        undefined ->
            {error, {invalid_codepoint, CP}};
        {Status, Replacement} ->
            status_replace(CP, Rest, Status, Replacement, Std3Rules, Transitional, Acc);
        Status ->
            status_replace(CP, Rest, Status, undefined, Std3Rules, Transitional, Acc)
    end;
uts46_remap([], _, _, Acc) ->
    {ok, Acc}.

status_replace(CP, Rest, 'V', _, Std3Rules, Transitional, Acc) ->
    uts46_remap(Rest, Std3Rules, Transitional, [CP | Acc]);
status_replace(CP, Rest, 'D', _, Std3Rules, false, Acc) ->
    uts46_remap(Rest, Std3Rules, false, [CP | Acc]);
status_replace(CP, Rest, '3', undefined, true, Transitional, Acc) ->
    uts46_remap(Rest, true, Transitional, [CP | Acc]);
status_replace(_, Rest, 'I', _, Std3Rules, Transitional, Acc) ->
    uts46_remap(Rest, Std3Rules, Transitional, Acc);
status_replace(CP, _, undefined, _, _, _, _) ->
    {error, {invalid_codepoint, CP}};
status_replace(_, Rest, 'M', Replacement, Std3Rules, Transitional, Acc) ->
    uts46_remap(Rest, Std3Rules, Transitional, [Replacement | Acc]);
status_replace(_, Rest, '3', Replacement, false, Transitional, Acc) ->
    uts46_remap(Rest, false, Transitional, [Replacement | Acc]);
status_replace(_, Rest, 'D', Replacement, Std3Rules, true, Acc) ->
    uts46_remap(Rest, Std3Rules, true, [Replacement | Acc]);
status_replace(CP, _, _, _, _, _, _) ->
    {error, {invalid_codepoint, CP}}.


is_ascii(Char) when is_integer(Char) ->
    Char >= 0 andalso Char =< 127;
is_ascii(_) ->
    false.


format(Format, Args) ->
    unicode:characters_to_list(io_lib:format(Format, Args)).

%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
%% @doc Punycode ([RFC 3492](http://tools.ietf.org/html/rfc3492)) implementation.

-module(punycode).


-export([encode/1, decode/1]).

-define(BASE,         36).
-define(TMIN,         1).
-define(TMAX,         26).
-define(SKEW,         38).
-define(DAMP,         700).
-define(INITIAL_BIAS, 72).
-define(INITIAL_N,    128).
-define(DELIMITER,    $-).


-define(MAX, 1 bsl 32 - 1).

-spec encode(String) -> Result
    when String :: string(),
         Result :: {ok, string()} | {error, overflow | badarg}.
%% @doc Convert Unicode to Punycode.
%%
%% Returns an overflow error on inputs that would take more than 63 encoded bytes,
%% the DNS limit on domain name labels.

encode(Input) ->
    InBounds = fun(C) -> case C < 16#80 of true -> {true, C}; false -> false end end,
    Filtered = lists:filtermap(InBounds, Input),
    Length = length(Filtered),
    Output =
        case Length > 0 of
            true  -> Filtered ++ [?DELIMITER];
            false -> Filtered
        end,
    encode(Input, Output, Length, Length, ?INITIAL_N, 0, ?INITIAL_BIAS).


encode(Input, Output, H, B, N, Delta, Bias) when H < length(Input) ->
    M = lists:min(lists:filter(fun(C) -> C >= N end, Input)),
    Delta1 =
        case (M - N) > ((?MAX - Delta) / (H + 1)) of
            false -> Delta + (M - N) * (H + 1);
            true  -> {error, overflow}
        end,
    {Output2, H2, Delta2, N2, Bias2} = encode1(Input, Output, H, B, M, Delta1, Bias),
    encode(Input, Output2, H2, B, N2, Delta2, Bias2);
encode(_, Output, _, _, _, _, _) ->
    {ok, Output}.

encode1([C | Rest], Output, H, B, N, Delta, Bias) when C < N ->
    Delta2 = Delta + 1,
    case Delta2 of
        0 -> {error, overflow};
        _ -> encode1(Rest, Output, H, B, N, Delta2, Bias)
    end;
encode1([C | Rest], Output, H, B, N, Delta, Bias) when C == N ->
    encode2(Rest, Output, H, B, N, Delta, Bias, Delta, ?BASE);
encode1([_ | Rest], Output, H, B, N, Delta, Bias) ->
    encode1(Rest, Output, H, B, N, Delta, Bias);
encode1([], Output, H, _, N, Delta, Bias) ->
    {Output, H, Delta + 1, N + 1, Bias}.

encode2(Rest, Output, H, B, N, Delta, Bias, Q, K) when K =< Bias ->
    encode3(Rest, Output, H, B, N, Delta, Bias, Q, K, ?TMIN);
encode2(Rest, Output, H, B, N, Delta, Bias, Q, K) when K >= (Bias + ?TMAX) ->
    encode3(Rest, Output, H, B, N, Delta, Bias, Q, K, ?TMAX);
encode2(Rest, Output, H, B, N, Delta, Bias, Q, K) ->
    encode3(Rest, Output, H, B, N, Delta, Bias, Q, K, K - Bias).

encode3(Rest, Output, H, B, N, Delta, _, Q, _, T) when Q < T ->
    case to_digit(Q) of
        {ok, CodePoint} ->
            Output2 = Output ++ [CodePoint],
            Bias2 = adapt(Delta, H + 1, H == B),
            Delta2 = 0,
            H2 = H + 1,
            encode1(Rest, Output2, H2, B, N, Delta2, Bias2);
        Error ->
            Error
    end;
encode3(Rest, Output, H, B, N, Delta, Bias, Q, K, T) ->
    case to_digit(T + ((Q - T) rem (?BASE - T))) of
        {ok, CodePoint} ->
            Output2 = Output ++ [CodePoint],
            Q2 = (Q - T) div (?BASE - T),
            encode2(Rest, Output2, H, B, N, Delta, Bias, Q2, K + ?BASE);
        Error ->
            Error
    end.


to_digit(V) when V >=  0, V =< 25 -> {ok, V + $a};
to_digit(V) when V >= 26, V =< 35 -> {ok, V - 26 + $0};
to_digit(_)                       -> {error, badarg}.


-spec decode(String) -> Result
    when String :: string(),
         Result :: {ok, string()} | {error, overflow | badarg}.
%% @doc Convert Punycode to Unicode.
%%
%% Returns an overflow or badarg error if malformed or overflow.
%% Overflow can only happen on inputs that take more than 63 encoded bytes,
%% the DNS limit on domain name labels.

decode(Input) ->
    {Output, NextIn} =
        case string:rstr(Input, [?DELIMITER]) of
            0 ->
                {"", Input};
            Pos ->
                Out = lists:sublist(Input, Pos - 1),
                In = lists:sublist(Input, Pos + 1, length(Input)),
                {Out, In}
        end,
    decode(NextIn, Output, ?INITIAL_N, ?INITIAL_BIAS, 0).

decode([], Output, _, _, _) ->
    {ok, Output};
decode(Input, Output, N, Bias, I) ->
    decode2(Input, Output, N, Bias, I, I, 1, ?BASE).

decode2(Input, Output, N, Bias, I0, OldI, Weight, K) when K =< Bias ->
    decode3(Input, Output, N, Bias, I0, OldI, Weight, K, ?TMIN);
decode2(Input, Output, N, Bias, I0, OldI, Weight, K) when K >= (Bias + ?TMAX) ->
    decode3(Input, Output, N, Bias, I0, OldI, Weight, K, ?TMAX);
decode2(Input, Output, N, Bias, I0, OldI, Weight, K) ->
    decode3(Input, Output, N, Bias, I0, OldI, Weight, K, K - Bias).

decode3([C | Rest], Output, N, Bias, I0, OldI, Weight, K, T) ->
    case digit(C) of
        {ok, Digit} -> decode4(Rest, Output, N, Bias, I0, OldI, Weight, K, T, Digit);
        Error       -> Error
    end.

decode4(Rest, Output, N, Bias, I0, OldI, Weight, K, T, Digit) ->
    case Digit > ((?MAX - I0 ) div Weight) of
        false ->
            I1 = I0 + (Digit * Weight),
            decode5(Rest, Output, N, Bias, I1, OldI, Weight, K, T, Digit);
        true ->
            {error, overflow}
    end.

decode5(Rest, Output, N, Bias, I1, OldI, Weight, K, T, Digit) ->
    case Digit < T of
        true ->
            Len = length(Output),
            case (I1 div (Len +1)) > (?MAX - N) of
                false ->
                    Bias2 = adapt(I1 - OldI, Len + 1, (OldI =:= 0)),
                    N2 = N + (I1 div (Len + 1)),
                    I2 = I1 rem (Len + 1),
                    decode6(Rest, Output, N2, I2, Bias2);
                true ->
                    {error, overflow}
            end;
        false ->
            case Weight > (?MAX  div (?BASE - T)) of
                false ->
                    NewWeight = Weight * (?BASE - T),
                    NewK = K + ?BASE,
                    decode2(Rest, Output, N, Bias, I1, OldI, NewWeight, NewK);
                true ->
                    {error, overflow}
            end
    end.

decode6(Rest, Output, N2, I2, Bias2) ->
    case insert(Output, N2, [], I2) of
        {ok, Output2} -> decode(Rest, Output2, N2, Bias2, I2 + 1);
        Error         -> Error
    end.


insert(Tail, CP, Head, 0) ->
    {ok, Head ++ [CP | Tail]};
insert([], _, _, I) when I > 0->
    {error, overflow};
insert([C | Tail], CP, Head, I) ->
    insert(Tail, CP, Head ++ [C], I - 1).


digit(C) when C >= $0, C =< $9 -> {ok, C - $0 + 26};
digit(C) when C >= $A, C =< $Z -> {ok, C - $A};
digit(C) when C >= $a, C =< $z -> {ok, C - $a};
digit(_)                       -> {error, badarg}.


adapt(Delta, NumPoints, FirstTime) ->
    Delta2 =
        case FirstTime of
            true  -> Delta div ?DAMP;
            false -> Delta div 2
        end,
    adapt(Delta2 + (Delta2 div NumPoints), 0).

adapt(Delta, K) ->
    case Delta > (((?BASE - ?TMIN) * ?TMAX) div 2) of
        true  -> adapt(Delta div (?BASE - ?TMIN), K + ?BASE);
        false -> K + (((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW))
    end.

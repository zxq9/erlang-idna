-module(punycode).

-export([encode/1,
         decode/1]).


%%============================================================================
%% Constants
%%============================================================================

-define(BASE, 36).
-define(TMIN, 1).
-define(TMAX, 26).
-define(SKEW, 38).
-define(DAMP, 700).
-define(INITIAL_BIAS, 72).
-define(INITIAL_N, 128).
-define(DELIMITER, $-).


-define(MAX, 1 bsl 32 - 1).
%%============================================================================
%% Encoding algorithm state
%%============================================================================

-record(encode, {n=?INITIAL_N, delta=0, bias=?INITIAL_BIAS, h, b}).
-record(decode, {n=?INITIAL_N, delta=0, bias=?INITIAL_BIAS, i=0, k, w}).

%%============================================================================
%% API
%%============================================================================

encode(Input) ->
  encode(Input, lists:reverse(lists:filter(fun(C) -> C < 16#80 end, Input))).


%%============================================================================
%% Helper functions
%%============================================================================

encode(Input, Basic) ->
  case length(Basic) of
    0 -> encode_whileloop(Input, [], #encode{h=0, b=0});
    N -> encode_whileloop(Input, [?DELIMITER|Basic], #encode{h=N, b=N})
  end.

encode_whileloop(Input, Output, State=#encode{h=H}) when H < length(Input) ->
  N = State#encode.n,
  M = lists:min(lists:filter(fun(C) -> C >= N end, Input)),
  Delta = State#encode.delta + (M - N) * (H + 1),
  {Output2, State2=#encode{delta=Delta2, n=N2}} = encode_foreachloop(Input, Output, State#encode{delta=Delta, n=M}),
  encode_whileloop(Input, Output2, State2#encode{delta=Delta2 + 1, n=N2 + 1});
encode_whileloop(_, Output, _) ->
  lists:reverse(Output).

encode_foreachloop([], Output, State) ->
  {Output, State};
encode_foreachloop([C|Input], Output, State=#encode{n=N, delta=Delta}) when C < N ->
  encode_foreachloop(Input, Output, State#encode{delta=Delta + 1});
encode_foreachloop([C|Input], Output, State=#encode{n=N, delta=Delta, h=H, b=B, bias=Bias}) when C =:= N ->
  {Output2, Q} = encode_forloop(Output, ?BASE, Delta, Bias),
  Bias2 = adapt(Delta, H + 1, H =:= B),
  encode_foreachloop(Input, [encode_digit(Q)|Output2], State#encode{delta=0, h=H + 1, bias=Bias2});
encode_foreachloop([_|Input], Output, State) ->
  encode_foreachloop(Input, Output, State).

encode_forloop(Output, K, Q, Bias) ->
  T = case K =< Bias of
        true ->
          ?TMIN;
        false ->
          case K >= (Bias + ?TMAX) of true -> ?TMAX; false -> (K - Bias) end
      end,
  case Q < T of
    true ->
      {Output, Q};
    false ->
      Digit = encode_digit(T + ((Q - T) rem (?BASE - T))),
      encode_forloop([Digit|Output], K + ?BASE, (Q - T) div (?BASE - T), Bias)
  end.

encode_digit(N) when N < 26 ->
  N + 22 + 75;
encode_digit(N) ->
  N + 22.


decode(Input) ->
  {Output, Input2} = case string:rstr(Input, [?DELIMITER]) of
             0 -> {"", Input};
             Pos ->
               {lists:sublist(Input, Pos - 1), lists:sublist(Input, Pos + 1, length(Input) )}
           end,
  decode(Input2, Output, ?INITIAL_N, ?INITIAL_BIAS, 0).


decode([], Output, _, _, _) -> Output;
decode(Input, Output, N, Bias, I) ->
  decode(Input, Output,  N, Bias, I, I, 1, ?BASE).

decode([C|Rest], Output, N, Bias, I0, OldI, Weight, K) ->
  Digit = digit(C),
  I1 = case Digit > ((?MAX - I0 ) div Weight) of
         false -> I0 + (Digit * Weight);
         true -> exit(overflow)
       end,

  T = if
        K =< Bias -> ?TMIN;
        K >= (Bias + ?TMAX) -> ?TMAX;
        true -> K - Bias
      end,
  case Digit < T of
    true ->
      Len = length(Output),
      Bias2 = adapt(I1 - OldI, Len + 1, (OldI == 0)),
      {N2, I2}= case (I1 div (Len +1)) > (?MAX - N) of
                  false ->
                    {N + I1 div (Len + 1), I1 rem (Len + 1)};
                  true ->
                    exit(overflow)
                end,

      {Head, Tail} = lists:split(I2, Output),
      Output2 = Head ++ [N2] ++ Tail,
      decode(Rest, Output2, N2, Bias2, I2+1);
    false ->
      case Weight > (?MAX  div (?BASE - T)) of
        false ->
          decode(Rest, Output, N, Bias, I1, OldI, Weight * (?BASE - T), K + ?BASE);
        true ->
          exit(overflow)
      end
  end.


digit(C) when C >= $0, C =< $9 -> C - $0 + 26;
digit(C) when C >= $A, C =< $Z -> C - $A;
digit(C) when C >= $a, C =< $z -> C - $a.


adapt(Delta, NumPoints, FirstTime) ->
  Delta2 = case FirstTime of
             true ->
               Delta div ?DAMP;
             false ->
               Delta div 2
           end,
  adapt(Delta2 + (Delta2 div NumPoints), 0).

adapt(Delta, K) ->
  case Delta > (((?BASE - ?TMIN) * ?TMAX) div 2) of
    true ->
      adapt(Delta div (?BASE - ?TMIN), K + ?BASE);
    false ->
      K + (((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW))
  end.
#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_mapping").



main(_) ->
  {ok, IM} = file:open("../uc_spec/IdnaMappingTable.txt", [read, raw, {read_ahead, 1000000}]),
  Data = foldl(fun parse_idna_mapping/2, [], IM),
  file:close(IM),

  %% Make module
  OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
  {ok, Out} = file:open(OutputPath, [write]),
  gen_file(Out, Data),
  ok = file:close(Out),
  ok.


parse_idna_mapping(Line0, Acc) ->
  [Line|_Comments] = tokens(Line0, "#"),
  case tokens(Line, ";") of
    [CodePoints, Status] ->
      [{to_range(CodePoints), to_atom(Status), undefined, undefined} | Acc];
    [CodePoints, Status, Mapping] ->
      [{to_range(CodePoints), to_atom(Status), to_mapping(Mapping), undefined} | Acc];
    [CodePoints, Status, Mapping, Idna2008Status] ->
      [{to_range(CodePoints), to_atom(Status), to_mapping(Mapping), to_atom(Idna2008Status)} | Acc]
  end.


-ifdef('OTP_RELEASE').
to_mapping(Mapping) ->
  [hex_to_int(C) || C <- string:lexemes(Mapping, " ")].
-else.
to_mapping(Mapping) ->
   [hex_to_int(C) || C <- string:strip(string:tokens(Mapping, " "), both)].
-endif.


to_range(CodePoints0) ->
   case tokens(CodePoints0, ".") of
     [CodePoint] ->
       {hex_to_int(CodePoint), undefined};
     [CodePoint1, "", CodePoint2] ->
       {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
   end.


gen_file(Fd, Data) ->
  gen_header(Fd),
  gen_utc46(Fd, Data),
  ok.


gen_header(Fd) ->
  io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
  io:put_chars(Fd, "%% see ../uc_spec/gen_unicode_mod.escript\n\n"),
  io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
  io:put_chars(Fd, "-export([uts46_map/1]).\n"),
  ok.

gen_utc46(Fd, Data) ->
  lists:foreach(fun({Cp, Status, Mapping, IStatus}) ->
                    io:format(Fd, "uts46_map~s {~p, ~p, ~p};~n", [gen_single_clause(Cp), Status, Mapping, IStatus])
                end,
                optimize_ranges(lists:sort(Data))),
  io:put_chars(Fd, "uts46_map(_) -> false."),
  ok.




gen_single_clause({R0, undefined}) ->
    io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
    io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0,R1]).




optimize_ranges(Rs0) ->
  PF = fun({{N, undefined}, _, _, _}) when is_integer(N) -> true;
          (_) -> false
       end,

  {Singles, Rs} = lists:partition(PF, Rs0),
  Singles ++ Rs.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef('OTP_RELEASE').
hex_to_int([]) -> [];
hex_to_int(HexStr) ->
    list_to_integer(string:trim(HexStr, both), 16).

to_atom(Str) ->
    list_to_atom(string:lowercase(string:trim(Str, both))).
-else.
hex_to_int([]) -> [];
hex_to_int(HexStr) ->
    list_to_integer(string:strip(HexStr, both), 16).

to_atom(Str) ->
    list_to_atom(string:to_lower(string:strip(Str, both))).
-endif.

foldl(Fun, Acc, Fd) ->
    Get = fun() -> file:read_line(Fd) end,
    foldl_1(Fun, Acc, Get).

foldl_1(_Fun, {done, Acc}, _Get) -> Acc;
foldl_1(Fun, Acc, Get) ->
    case Get() of
        eof -> Acc;
        {ok, "#" ++ _} -> %% Ignore comments
            foldl_1(Fun, Acc, Get);
        {ok, "\n"} -> %% Ignore empty lines
            foldl_1(Fun, Acc, Get);
        {ok, Line} ->
            foldl_1(Fun, Fun(Line, Acc), Get)
    end.



%% Differs from string:tokens, it returns empty string as token between two delimiters
tokens(S, [C]) ->
    tokens(lists:reverse(S), C, []).

tokens([Sep|S], Sep, Toks) ->
    tokens(S, Sep, [[]|Toks]);
tokens([C|S], Sep, Toks) ->
    tokens_2(S, Sep, Toks, [C]);
tokens([], _, Toks) ->
    Toks.

tokens_2([Sep|S], Sep, Toks, Tok) ->
    tokens(S, Sep, [Tok|Toks]);
tokens_2([C|S], Sep, Toks, Tok) ->
    tokens_2(S, Sep, Toks, [C|Tok]);
tokens_2([], _Sep, Toks, Tok) ->
    [Tok|Toks].

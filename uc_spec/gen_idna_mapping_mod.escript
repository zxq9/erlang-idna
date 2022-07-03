#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_mapping").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str), string:trim(Str, both)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-define(lower(C), string:lowercase(C)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str), string:strip(Str, both)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-define(lower(C), string:to_lower(C)).
-endif.

-define(UTS46_STATUSES,
        #{"valid"                  => {'V', false},
          "ignored"                => {'I', false},
          "mapped"                 => {'M', true},
          "deviation"              => {'D', true},
          "disallowed"             => {'X', false},
          "disallowed_STD3_valid"  => {'3', false},
          "disallowed_STD3_mapped" => {'3', true}}).



main(_) ->
    Table = "../uc_spec/IdnaMappingTable.txt",
    {ok, IM} = file:open(Table, [read, raw, {read_ahead, 1000000}]),
    Data = read_data(fun parse_idna_mapping/2, [], IM),
    ok = file:close(IM),

    %% Make module
    OutputPath = filename:join(["..", "src", ?MOD ++ ".erl"]),
    {ok, Out} = file:open(OutputPath, [write]),
    ok = gen_file(Out, Data),
    ok = file:close(Out).


parse_idna_mapping(Line, Acc) ->
    [Code | _Comments] = tokens(Line, "#"),
    case tokens(Code, ";") of
        [CodePoints, Status] ->
            Mapped = {?trim(Status), undefined, undefined},
            Parsed = {to_range(CodePoints), Mapped},
            [Parsed | Acc];
        [CodePoints, Status, Mapping] ->
            Mapped = {?trim(Status), to_mapping(Mapping), undefined},
            Parsed = {to_range(CodePoints), Mapped},
            [Parsed | Acc];
        [CodePoints, Status, Mapping, Idna2008Status] ->
            Mapped = {?trim(Status), to_mapping(Mapping), to_atom(Idna2008Status)},
            Parsed = {to_range(CodePoints), Mapped},
            [Parsed | Acc]
    end.


to_mapping(Mapping) ->
    [hex_to_int(C) || C <- ?lexemes(Mapping, " ")].


to_range(CodePoints) ->
    case tokens(CodePoints, ".") of
        [CodePoint] ->
            {hex_to_int(CodePoint), undefined};
        [CodePoint1, "", CodePoint2] ->
            {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
    end.


gen_file(Fd, Data) ->
    ok = gen_header(Fd),
    gen_utc46(Fd, Data).


gen_header(Fd) ->
    Header =
        "%%\n%% this file is generated do not modify\n"
        "%% see ../uc_spec/gen_idna_mapping.escript\n\n"
        "-module(" ++ ?MOD ++").\n"
        "-compile(compressed).\n"
        "-export([uts46_map/1]).\n\n\n",
    io:put_chars(Fd, Header).


gen_utc46(Fd, Data) ->
    Print =
        fun({CP, {S, M, _}}) ->
            {Status, Mapping} = maps:get(S, ?UTS46_STATUSES),
            Clause = gen_single_clause(CP),
            case Mapping of
                true ->
                    Format = "uts46_map~ts {~tp, ~tw};\n",
                    io:format(Fd, Format, [Clause, Status, M]);
                false ->
                    Format = "uts46_map~ts ~tp;\n",
                    io:format(Fd, Format, [Clause, Status])
            end
        end,
    ok = lists:foreach(Print, optimize_ranges(lists:sort(Data))),
    io:put_chars(Fd, "uts46_map(_) -> undefined.\n").


gen_single_clause({R0, undefined}) ->
    io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
    io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0, R1]).


optimize_ranges(Rs0) ->
  PF =
        fun
            ({{N, undefined}, _}) when is_integer(N) -> true;
            (_)                                      -> false
        end,
  {Singles, Rs} = lists:partition(PF, Rs0),
  Singles ++ Rs.



%%%

hex_to_int([]) ->
    [];
hex_to_int(HexStr) ->
    list_to_integer(?trim(HexStr), 16).


to_atom(Str) ->
    list_to_atom(?lower(?trim(Str))).


read_data(Fun, Acc, Fd) ->
    Get = fun() -> file:read_line(Fd) end,
    fold(Fun, Acc, Get).

fold(_, {done, Acc}, _) ->
    Acc;
fold(Fun, Acc, Get) ->
    case Get() of
        eof            -> Acc;
        {ok, "#" ++ _} -> fold(Fun, Acc, Get); %% Ignore comments
        {ok, "\n"}     -> fold(Fun, Acc, Get); %% Ignore empty lines
        {ok, Line}     -> fold(Fun, Fun(Line, Acc), Get)
    end.


%% Differs from string:tokens, it returns empty string as token between two delimiters
tokens(S, [C]) ->
    tokens(lists:reverse(S), C, []).

tokens([Sep | S], Sep, Toks) ->
    tokens(S, Sep, [[] | Toks]);
tokens([C | S], Sep, Toks) ->
    tokens_2(S, Sep, Toks, [C]);
tokens([], _, Toks) ->
    Toks.

tokens_2([Sep|S], Sep, Toks, Tok) ->
    tokens(S, Sep, [Tok | Toks]);
tokens_2([C|S], Sep, Toks, Tok) ->
    tokens_2(S, Sep, Toks, [C | Tok]);
tokens_2([], _Sep, Toks, Tok) ->
    [Tok | Toks].

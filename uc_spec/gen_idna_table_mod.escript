#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_table").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(trim(Str), string:trim(Str, both)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-else.
-define(trim(Str), string:strip(Str, both)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-endif.


main(_) ->
    Table = "../uc_spec/idna-table.txt",
    {ok, IM} = file:open(Table, [read, raw, {read_ahead, 1000000}]),
    Data = read_data(fun parse_idna_table/2, [], IM),
    file:close(IM),

    %% Make module
    OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
    {ok, Out} = file:open(OutputPath, [write]),
    ok = gen_file(Out, Data),
    ok = file:close(Out).


parse_idna_table(Line0, Acc) ->
    [Line | _Comments] = tokens(Line0, "#"),
    [CodePoints, Status] = tokens(Line, ";"),
    [{to_range(CodePoints), to_atom(Status)} | Acc].


gen_file(Fd, Data) ->
    ok = gen_header(Fd),
    ok = gen_disallowed_p(Fd),
    ok = gen_contextj_p(Fd),
    ok = gen_contexto_p(Fd),
    ok = gen_unassigned_p(Fd),
    ok = gen_valid_p(Fd),
    ok = gen_lookup(Fd, Data).


gen_header(Fd) ->
    Header =
        "%%\n%% this file is generated do not modify\n"
        "%% see ../uc_spec/gen_idna_table.escript\n\n"
        "-module(" ++ ?MOD ++").\n"
        "-compile(compressed).\n"
        "-export([lookup/1,\n"
        "         disallowed_p/1,\n"
        "         contextj_p/1, contexto_p/1,\n"
        "         unassigned_p/1,\n"
        "         valid_p/1]).\n\n\n",
    ok = io:put_chars(Fd, Header).


gen_disallowed_p(Fd) ->
    io:put_chars(Fd, "disallowed_p(CP) -> lookup(CP) == 'DISALLOWED'.\n\n\n").


gen_contextj_p(Fd) ->
    io:put_chars(Fd, "contextj_p(CP) -> lookup(CP) == 'CONTEXTJ'.\n\n\n").


gen_contexto_p(Fd) ->
    io:put_chars(Fd, "contexto_p(CP) -> lookup(CP) == 'CONTEXTO'.\n\n\n").


gen_unassigned_p(Fd) ->
    io:put_chars(Fd, "unassigned_p(CP) -> lookup(CP) == 'UNASSIGNED'.\n\n\n").


gen_valid_p(Fd) ->
    io:put_chars(Fd, "valid_p(CP) -> lookup(CP) == 'PVALID'.\n\n\n").


gen_lookup(Fd, Data) ->
    Lookup =
        fun({Cp, Class}) ->
            io:format(Fd, "lookup~s ~p;~n", [clause(Cp), Class])
        end,
    lists:foreach(Lookup, optimize_ranges(lists:sort(Data))),
    ok = io:put_chars(Fd, "lookup(_) -> 'UNASSIGNED'.").


clause({FirstCP, LastCP}) ->
    io_lib:format("(CP) when ~7w =< CP, CP =< ~7w ->", [FirstCP, LastCP]);
clause(CodePoint) ->
    io_lib:format("(~w) ->", [CodePoint]).


optimize_ranges(CodeValues) ->
    Filter =
        fun
            ({N, _}) when is_integer(N) -> true;
            (_)                         -> false
        end,
    {Singles, Rs} = lists:partition(Filter, CodeValues),
    Singles ++ Rs.


to_range(CodePoints) ->
    case tokens(CodePoints, ".") of
        [CP]                  -> hex_to_int(CP);
        [FirstCP, "", LastCP] -> {hex_to_int(FirstCP), hex_to_int(LastCP)}
    end.


hex_to_int([]) ->
    [];
hex_to_int(HexStr) ->
    list_to_integer(?trim(HexStr), 16).


to_atom(Str) ->
  list_to_atom(?trim(Str)).


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

tokens_2([Sep | S], Sep, Toks, Tok) ->
    tokens(S, Sep, [Tok | Toks]);
tokens_2([C | S], Sep, Toks, Tok) ->
    tokens_2(S, Sep, Toks, [C | Tok]);
tokens_2([], _Sep, Toks, Tok) ->
    [Tok | Toks].

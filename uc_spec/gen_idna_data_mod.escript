#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_data").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str, Dir), string:trim(Str, Dir)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-define(lower(C), string:lowercase(C)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str, Dir), string:strip(Str, Dir)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-define(lower(C), string:to_lower(C)).
-endif.

%% Default `Bidi_Class` for unassigned codepoints.
%%
%% Ref: <https://www.unicode.org/Public/UNIDATA/extracted/DerivedBidiClass.txt>
-define(BIDI_CLASS_DEFAULTS,
        [{{16#0600, 16#07BF}, "AL"},
         {{16#0860, 16#086F}, "AL"},
         {{16#08A0, 16#08FF}, "AL"},
         {{16#FB50, 16#FDCF}, "AL"},
         {{16#FDF0, 16#FDFF}, "AL"},
         {{16#FE70, 16#FEFF}, "AL"},
         {{16#00010D00, 16#00010D3F}, "AL"},
         {{16#00010F30, 16#00010F6F}, "AL"},
         {{16#0001EC70, 16#0001ECBF}, "AL"},
         {{16#0001EE00, 16#0001EEFF}, "AL"},
         %% Arabic, Syriac, and Thaana blocks, among others
         {{16#0590, 16#05FF}, "R"},
         {{16#07C0, 16#085F}, "R"},
         {{16#0870, 16#089F}, "R"},
         {{16#FB1D, 16#FB4F}, "R"},
         {{16#00010800, 16#00010CFF}, "R"},
         {{16#00010D40, 16#00010F2F}, "R"},
         {{16#00010F70, 16#00010FFF}, "R"},
         {{16#0001E800, 16#0001EC6F}, "R"},
         {{16#0001ECC0, 16#0001EDFF}, "R"},
         {{16#0001EF00, 16#0001EFFF}, "R"},
         %% Hebrew, NKo, and Phoenician blocks, among others.
         {{16#20A0, 16#20CF}, "ET"}]).
         %% Currency Symbols block.


main(_) ->
    Table = "../uc_spec/UnicodeData.txt",
    {ok, IM} = file:open(Table, [read, raw, {read_ahead, 1000000}]),
    Data = read_data(fun parse_unicode_data/2, [], IM),
    ok = file:close(IM),

    Shaping = "../uc_spec/ArabicShaping.txt",
    {ok, AS} = file:open(Shaping, [read, raw, {read_ahead, 1000000}]),
    JoiningTypes = read_data(fun parse_as/2, [], AS),
    ok = file:close(AS),

    ScriptsTxt = "../uc_spec/Scripts.txt",
    {ok, ScriptsF} = file:open(ScriptsTxt, [read, raw, {read_ahead, 1000000}]),
    Scripts = read_data(fun parse_scripts/2, [], ScriptsF),
    ok = file:close(ScriptsF),

    %% Make module
    OutputPath = filename:join(["..", "src", ?MOD ++ ".erl"]),
    {ok, Out} = file:open(OutputPath, [write]),
    ok = gen_file(Out, Data, JoiningTypes, Scripts),
    ok = file:close(Out).


gen_file(Fd, Data, JoiningTypes, Scripts) ->
  ok = gen_header(Fd),
  ok = gen_bidirectional(Fd),
  ok = gen_lookup(Fd, Data),
  ok = gen_joining_types(Fd, JoiningTypes),
  ok = gen_scripts_types(Fd, Scripts).


gen_header(Fd) ->
    Header =
        "%%\n%% this file is generated do not modify\n"
        "%% see ../uc_spec/gen_idna_data_mod.escript\n\n"
        "-module(" ++ ?MOD ++").\n"
        "-compile(compressed).\n"
        "-export([lookup/1, joining_types/1, scripts/1, bidirectional/1]).\n",
    io:put_chars(Fd, Header).


gen_bidirectional(Fd) ->
    Bidirectional =
        "\n\nbidirectional(CP) ->\n"
        "    case lookup(CP) of \n"
        "        {_, C} -> C;\n"
        "        false  -> bidi(CP)\n"
        "    end.\n\n",
    ok = io:put_chars(Fd, Bidirectional),
    Bidi = fun({CP, Class}) -> io:format(Fd, "bidi~s ~p;~n", [clause(CP), Class]) end,
    lists:foreach(Bidi, lists:sort(?BIDI_CLASS_DEFAULTS)),
    io:put_chars(Fd, "bidi(_)                                    -> \"L\".\n\n\n").


gen_lookup(Fd, Data) ->
    Lookup = fun ({CP, TP}) -> io:format(Fd, "lookup~s ~p;~n", [clause(CP), TP]) end,
    ok = lists:foreach(Lookup, condense(Data)),
    io:put_chars(Fd, "lookup(_) -> false.\n\n\n").

condense(Data) ->
    [{CP, TP} | Rest] = lists:sort(Data),
    condense(Rest, CP, CP, TP, []).

condense([{CP, TP} | Data], StartCP, LastCP, TP, Acc) when CP =:= LastCP + 1 ->
    condense(Data, StartCP, CP, TP, Acc);
condense([{CP, TP} | Data], StartCP, LastCP, TP, Acc) when CP > LastCP + 1 ->
    NewAcc =
        case StartCP + 1 =:= LastCP of
            true  -> [{LastCP, TP}, {StartCP, TP} | Acc];
            false -> [{{StartCP, LastCP}, TP} | Acc]
        end,
    condense(Data, CP, CP, TP, NewAcc);
condense([{CP, TP} | Data], StartCP, StartCP, TP, Acc) ->
    condense(Data, StartCP, CP, TP, Acc);
condense([{CP, TP} | Data], OldCP, OldCP, OldTP, Acc) ->
    condense(Data, CP, CP, TP, [{OldCP, OldTP} | Acc]);
condense([{CP, TP} | Data], StartCP, LastCP, OldTP, Acc) ->
    NewAcc =
        case StartCP + 1 =:= LastCP of
            true  -> [{LastCP, OldTP}, {StartCP, OldTP} | Acc];
            false -> [{{StartCP, LastCP}, OldTP} | Acc]
        end,
    condense(Data, CP, CP, TP, NewAcc);
condense([], CP, CP, TP, Acc) ->
    lists:reverse([{CP, TP} | Acc]);
condense([], StartCP, LastCP, TP, Acc) ->
    NewAcc =
        case StartCP + 1 =:= LastCP of
            true  -> [{LastCP, TP}, {StartCP, TP} | Acc];
            false -> [{{StartCP, LastCP}, TP} | Acc]
        end,
    lists:reverse(NewAcc).


gen_joining_types(Fd, JoiningTypes) ->
    JoiningType = 
        fun ({CP, JT}) ->
            io:format(Fd, "joining_types~s ~p;~n", [clause(CP), ?trim(JT, both)])
        end,
    ok = lists:foreach(JoiningType, condense(JoiningTypes)),
    io:put_chars(Fd, "joining_types(_) -> undefined.\n\n\n").


gen_scripts_types(Fd, Scripts) ->
    ScriptType =
        fun({CP, JT}) ->
            io:format(Fd, "scripts~s ~p;~n", [clause(CP), ?lower(?trim(JT, both))])
        end,
    ok = lists:foreach(ScriptType, optimize_ranges(lists:sort(Scripts))),
    io:put_chars(Fd, "scripts(_) -> false.\n\n").


optimize_ranges(CodeValues) ->
    Filter =
        fun
            ({N, _}) when is_integer(N) -> true;
            (_)                         -> false
        end,
    {Singles, Rs} = lists:partition(Filter, CodeValues),
    Singles ++ Rs.


clause({FirstCP, LastCP}) ->
    io_lib:format("(CP) when ~7w =< CP, CP =< ~7w ->", [FirstCP, LastCP]);
clause(CodePoint) ->
    io_lib:format("(~w) ->", [CodePoint]).



parse_unicode_data(Line, Acc) ->
    Chunk = ?chomp(Line),
    [CodePoint, _Name, Cat, _Class, BiDi | _] = tokens(Chunk, ";"),
    [{hex_to_int(CodePoint), {?trim(Cat, both), ?trim(BiDi, both)}} | Acc].

parse_as(Line0, Acc) ->
    Line = ?chomp(Line0),
    case tokens(Line, ";") of
        [CodePoint, _, JT | _] -> [{hex_to_int(CodePoint), ?trim(JT, both) } | Acc];
        _                      -> Acc
    end.

parse_scripts(Line, Acc) ->
    [Code | _Comments] = tokens(Line, "#"),
    [CodePoints, Hairy] = tokens(Code, ";"),
    Script = ?trim(Hairy, both),
    Scripts = ["Greek", "Han", "Hebrew", "Hiragana", "Katakana"],
    case lists:member(Script, Scripts) of
        true  -> [{to_range(CodePoints), Script} | Acc];
        false -> Acc
    end.


to_range(CodePoints) ->
    case tokens(CodePoints, ".") of
        [CP]                  -> hex_to_int(CP);
        [FirstCP, "", LastCP] -> {hex_to_int(FirstCP), hex_to_int(LastCP)}
    end.


hex_to_int([]) ->
    [];
hex_to_int(HexStr) ->
    list_to_integer(?trim(HexStr, both), 16).


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

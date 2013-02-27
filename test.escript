#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin"),
  inets:start(),
  idna:start(),
  punycode_encode_tests(),
  idna_to_ascii_tests().

punycode_encode_tests() ->
  test_each("test/punycode_*", fun (Test) ->
    {proplists:get_value(punycode, Test), punycode:encode(proplists:get_value(unicode, Test))}
  end).

idna_to_ascii_tests() ->
  test_each("test/idna_*", fun (Test) ->
    {proplists:get_value(output, Test), idna:to_ascii(xmerl_ucs:from_utf8(proplists:get_value(input, Test)))}
  end).

test_each(FilePattern, Fun) ->
  test_each(filelib:wildcard(FilePattern), Fun, 0).

test_each([], _, N) ->
  io:format("~p tests passed~n", [N]);
test_each([File | Files], Fun, N) ->
  {ok, Test} = file:consult(File),
  {Output, ReturnValue} = Fun(Test),
  case ReturnValue of
    Output ->
      test_each(Files, Fun, N + 1);
    _ ->
      io:format("~s~n", [File]),
      io:format("returned: ~p~n", [ReturnValue]),
      io:format("expected: ~p~n", [Output]),
      halt(1)
  end.

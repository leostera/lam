% Source code generated with Caramel.
-module(grep).

-export([main/1]).

print_matches(Word, [Word | T], File) ->
  io:format("~p: ~p\n", [File, Word]),
  print_matches(Word, T, File);
print_matches(Word, [_ | T], File)->
  print_matches(Word, T, File);
print_matches(_, _, _) -> ok.

count(Top, File, Word) ->
  erlang:spawn(fun () ->
    {ok, Data} = file:read_file(File),
    Words = binary:split(Data, <<" ">>, [global]),
    print_matches(Word, Words, File),
    Top ! {word_count, File, length(Words)}
  end).

count_all([]) -> ok;
count_all([_|T]) ->
  receive
    _ -> count_all(T)
  after
    1000 -> timeout
  end.

main([Word | Files]) ->
  Top = self(),
  Run = fun (File) -> count(Top, File, Word) end,
  lists:foreach(Run, Files),
  count_all(Files).

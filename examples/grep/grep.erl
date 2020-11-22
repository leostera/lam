% Source code generated with Caramel.
-module(grep).

-export([main/1]).

print_matches(Word, [Word | T], File) ->
  io:format("~p: ~p", [File, Word]),
  print_matches(Word, T, File);
print_matches(Word, [_ | T], File)->
  print_matches(Word, T, File);
print_matches(_, _, _) -> ok.

count(File, Word) ->
  {ok, Data} = file:read_file(File),
  Words = binary:split(Data, <<" ">>, [global]),
  print_matches(Word, Words, File).

main([Word | Files]) ->
  Run = fun (File) -> count(File, Word) end,
  lists:foreach(Run, Files).

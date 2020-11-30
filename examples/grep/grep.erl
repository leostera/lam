% Source code generated with Caramel.
-module(grep).

-export([main/1]).

print_matches(Word, [Word | T], File) ->
  io:format(<<"~p | ~p: ~p\n">>, [self(), File, Word]),
  print_matches(Word, T, File);
print_matches(Word, [_ | T], File)->
  print_matches(Word, T, File);
print_matches(_, _, _) -> ok.

count(Top, File, Word) ->
  erlang:spawn(fun () ->
    {ok, Data} = file:read_file(File),
    Words = binary:split(Data, [<<" ">>, <<"\n">>], [global]),
    print_matches(Word, Words, File),
    Top ! done
  end).

count_all([]) -> ok;
count_all([_|T]) -> receive done -> count_all(T) end.

main([Word | Files]) ->
  Top = self(),
  Run = fun (File) -> count(Top, File, binary:list_to_bin(Word)) end,
  lists:foreach(Run, Files),
  count_all(Files).

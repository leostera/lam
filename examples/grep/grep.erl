% Source code generated with Caramel.
-module(grep).

-export([count/3]).
-export([count_all/2]).
-export([count_aux/2]).
-export([main/1]).

-spec count_aux(any(), any()) -> integer().
count_aux(Word, Data) -> 0.

-spec count(any(), any(), binary()) -> ok.
count(Top, Word, File) ->
  erlang:spawn(fun
  (_self, _recv) ->
  case file:read_file(File) of
    {ok, Data} -> count_aux(Word, Data)
  end
end),
  ok.

-spec count_all(integer(), integer()) -> integer().
count_all(N, Acc) ->
  case erlang:'=:='(N, 0) of
    true -> Acc;
    false -> case process:recv(infinity) of
  {some, X} -> count_all(erlang:'-'(N, 1), erlang:'+'(Acc, X))
end
  end.

-spec main(list(binary())) -> ok.
main([Word | Files]) ->
  Top = erlang:self(),
  lists:foreach(fun
  (File) -> count(Top, Word, File)
end, Files),
  Total = count_all(erlang:length(Files), 0),
  io:format(<<"Found ~p occurrences of">>, [Total | []]),
  io:format(<<"~p">>, [Word | []]).



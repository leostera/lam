% Source code generated with Caramel.
-module(grep).

-export([main/1]).

count(File, Word, _Top) ->
  erlang:spawn(
    fun () ->
        io:format(<<"I'm: ~p, and will read: ~p">>, [self(), File])
        % case file:read_file(File) of
        %   {ok, Data} -> C = count_aux(Word, Data),,
        % end
        % erlang:send(Top, C)
    end),
  ok.

main([Word | Files]) ->
  Top = erlang:self(),
  Run = fun (File) -> count(File, Word, Top) end,
  lists:foreach(Run, Files).

%% count_all(0, Acc) -> Acc;
%% count_all(N, Acc) -> receive X -> count_all(N - 1, Acc + X) end.
%%   Total = count_all(erlang:length(Files), 0),
%%   io:format(<<"Found ~p occurrences of ~p">>, [Total, Word]).

-module(send).

-export([main/1]).

main([Msg|_]) ->
  Top = self(),
  Top ! Msg,
  receive
    X -> io:format(<<"~p">>, [X])
  end.

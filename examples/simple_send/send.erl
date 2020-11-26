-module(send).

-export([main/1]).

main(_) ->
  Top = self(),
  Top ! ok,
  receive
    X -> io:format(<<"~p">>, [X])
  end.

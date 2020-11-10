% Source code generated with Caramel.
-module(hello_joe).

-export([main/0]).

-spec main() -> ok.
main() ->
  Str = <<"Hello, Joe!">>,
  io:format(<<"~p">>, [Str | []]).



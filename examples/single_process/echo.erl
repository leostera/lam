% Source code generated with Caramel.
-module(echo).

-export([main/1]).

-spec main(list(any())) -> ok.
main([X | _]) -> io:format(<<"~p">>, [X | []]).



-module(hello_joe).

-export([main/1]).

main(_) -> io:format(<<"~p">>, [<<"Hello, Joe!">>]).



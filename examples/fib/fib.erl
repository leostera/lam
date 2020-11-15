% Source code generated with Caramel.
-module(fib).

-export([fib/3]).
-export([main/1]).

-spec fib(integer(), integer(), integer()) -> ok.
fib(N, B, A) ->
  case erlang:'=<'(N, 0) of
    true -> io:format(<<"~p">>, [A | []]);
    false -> fib(erlang:'-'(N, 1), erlang:'+'(A, B), B)
  end.

-spec main(list(binary())) -> ok.
main([Arg | []]) -> fib(erlang:list_to_integer(Arg), 1, 0).



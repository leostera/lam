-module(expr_arithmetic).

-export([main/1]).

main(_) ->
  A = 2112,
  A2 = 7,
  B = true,
  B2 = false,
  C = [a_list],
  io:format("~p\n", [[
   {<<"Unary ops">>,
    +A,
    -A,
    bnot A,
    not B
   },
   {
    <<"Left associativity">>,
    A * 1 * 2,
    A / 1 / 2,
    B and true and false,
    B or false or true,
    B xor false xor true,
    A band 1 band 2,
    A div 1 div 2,
    A rem 1 rem 2,
    A + 1 + 2,
    A - 1 - 2,
    A bor 1 bor 2,
    A bsl 1 bsl 2,
    A bsr 1 bsr 2,
    A bxor 1 bxor 2
   },
   {
    <<"Right associativity">>,
    [A] ++ [B] ++ C,
    [A] -- [B] -- C
   },
   {
    <<"Operator precedence">>,
    1 - - - 2,
    1 - - - 2,
    1 + - + 2
   },
   {
    <<"Binary operations">>,
    A - A2,
    A + A2,
    A * A2,
    [A] ++ [A2],
    [A] -- [A2],
    A / A2,
    A /= A2,
    A < A2,
    A =/= A2,
    A =:= A2,
    A =< A2,
    A == A2,
    A > A2,
    A >= A2,
    B and B2,
    A band A2,
    A bor A2,
    A bsl A2,
    A bsr A2,
    A bxor A2,
    A div A2,
    B or B2,
    A rem A2,
    B xor B2
   }
  ]]).

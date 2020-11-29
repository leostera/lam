-module(literals).
-export([main/1]).

main(_) ->
  Atom = ok,
  String = "hello",
  Binary = <<"world">>,
  SmallInt = 10,
  MediumInt = 10100,
  BigInt = 12345678901,
  Bool = true,
  List = [1,2,[3]],
  Map = #{ hello => world },
  Float = ok,
  Tuple = ok,
  io:format(<<"~p">>, [Atom, String, Binary, SmallInt, MediumInt, BigInt, Bool, List, Map, Float, Tuple]).

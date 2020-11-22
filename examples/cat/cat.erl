-module(cat).

-export([main/1]).

main([]) -> ok;
main([File|T]) ->
  io:format("~p\n", [file:read_file(File)]),
  main(T).

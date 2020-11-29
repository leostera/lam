-module(fib).

-export([main/1]).

add_entry(DomNode, Text) ->
  NewNode = dom_document:create_element(<<"div">>),
  dom_element:set_inner_text(NewNode, Text),
  dom_element:append_child(DomNode, NewNode).

fib(Node, N, B, A) ->
  case erlang:'=<'(N, 0) of
    true -> dom_element:set_inner_text(Node, erlang:integer_to_binary(A));
    false -> fib(Node, erlang:'-'(N, 1), erlang:'+'(A, B), B)
  end.

main(_) ->
  io:format(<<"Starting...">>, []),
  Node = dom_document:get_element_by_id(<<"lam">>),

  T0 = date:now(),
  add_entry(Node, erlang:integer_to_binary(T0)),

  % work!
  NumberNode = dom_document:create_element(<<"div">>),
  dom_element:set_inner_text(NumberNode, <<"unstarted">>),
  dom_element:append_child(Node, NumberNode),
  fib(NumberNode, 1000, 1, 0),

  T1 = erlang:'-'(date:now(), T0),
  add_entry(Node, erlang:integer_to_binary(T1)).

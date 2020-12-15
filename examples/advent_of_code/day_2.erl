% Source code generated with Caramel.
-module(day_2).
-export_type([password_req/0]).
-export_type([position_counter/0]).
-export_type([validation_result/0]).

-export([count_all/2]).
-export([count_min_max/2]).
-export([count_valid/3]).
-export([is_in_position/2]).
-export([run/0]).

-type password_req() :: #{ letter => binary()
                         , min => integer()
                         , max => integer()
                         }.

-type validation_result() :: {valid, integer()}
                           | {invalid, integer()}
                           .

-type position_counter() :: found_first
                          | found_last
                          | not_found
                          | too_many
                          .

-spec count_min_max(password_req(), binary()) -> validation_result().
count_min_max(#{ letter := Letter, min := Min, max := Max }, Pwd) ->
  Parts = binary:bin_to_list(Pwd),
  Result = lists:foldl(fun
  (C, Acc) ->
  case erlang:'=:='(Letter, binary:list_to_bin([C | []])) of
    true -> erlang:'+'(Acc, 1);
    false -> Acc
  end
end, 0, Parts),
  case erlang:'and'(erlang:'>='(Result, Min), erlang:'=<'(Result, Max)) of
    true -> {valid, Result};
    false -> {invalid, Result}
  end.

-spec is_in_position(password_req(), binary()) -> validation_result().
is_in_position(#{ letter := Letter, min := Min, max := Max }, Pwd) ->
  Parts = binary:bin_to_list(Pwd),
  {Result, Idx} = lists:foldl(fun
  (C, {Counter, Idx}) ->
  Same_char = erlang:'=:='(Letter, binary:list_to_bin([C | []])),
  case Counter of
    not_found -> case erlang:'and'(erlang:'=:='(Idx, Min), Same_char) of
  true -> {found_first, erlang:'+'(Idx, 1)};
  false -> case erlang:'and'(erlang:'=:='(Idx, Max), Same_char) of
  true -> {found_last, erlang:'+'(Idx, 1)};
  false -> {not_found, erlang:'+'(Idx, 1)}
end
end;
    found_first -> case erlang:'and'(erlang:'=:='(Idx, Max), Same_char) of
  true -> {too_many, 0};
  false -> {found_first, erlang:'+'(Idx, 1)}
end;
    found_last -> case erlang:'and'(erlang:'=:='(Idx, Min), Same_char) of
  true -> {too_many, 0};
  false -> {found_last, erlang:'+'(Idx, 1)}
end;
    too_many -> {too_many, 0}
  end
end, {not_found, 1}, Parts),
  case Result of
    found_first -> {valid, Idx};
    found_last -> {valid, Idx};
    _ -> {invalid, 0}
  end.

-spec count_valid(A, list(B), fun((A, B) -> validation_result())) -> integer().
count_valid(Req, Pwds, Run_validation) ->
  Results = lists:map(fun
  (Pwd) -> Run_validation(Req, Pwd)
end, Pwds),
  lists:foldl(fun
  (R, Acc) ->
  case R of
    {valid, _} -> erlang:'+'(Acc, 1);
    _ -> Acc
  end
end, 0, Results).

-spec count_all(fun((A, B) -> validation_result()), list({A, list(B)})) -> integer().
count_all(Validation, Data) -> lists:foldl(fun
  ({R, Pwds}, Acc) -> erlang:'+'(count_valid(R, Pwds, Validation), Acc)
end, 0, Data).

-spec run() -> list(option:t(integer())).
run() ->
  Data = [
          { #{ letter => <<"s">> , min => 3 , max => 6 },
            [<<"ssdsssss">> ]
          },
          { #{ letter => <<"f">> , min => 17 , max => 19 },
            [<<"cnffsfffzhfnsffttms">>]
          }
         ],
  [{some, count_all(fun count_min_max/2, Data)} | [{some, count_all(fun is_in_position/2, Data)} | []]].



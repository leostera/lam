%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(lists).

-export([foldl/3, foreach/2, map/2]).
-export([main/1]).

-spec map(Fun, List1) -> List2 when
      Fun :: fun((A) -> B),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

-spec foreach(Fun, List) -> ok when
      Fun :: fun((Elem :: T) -> term()),
      List :: [T],
      T :: term().

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(_, []) -> ok.

-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, [Hd|Tail]) -> foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

main(C) ->
  X = lists:foldl(fun (A, B) ->
                      io:format("~p", [C]),
                      A + B
                  end, 0, [1,2,3]),
  io:format(<<"~p\n">>, [X]).



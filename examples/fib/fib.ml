let rec fib n b a =
  match n <= 0 with
  | true -> Io.format "~p" [a]
  | false -> fib (n - 1) (a + b) b

let main (arg :: []) = fib (Erlang.list_to_integer arg) 1 0

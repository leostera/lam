let rec fib n b a =
  match n <= 0 with
  | true -> Io.format "~p" [a]
  | false -> fib (n - 1) (a + b) b

let main () = fib 2048 1 0

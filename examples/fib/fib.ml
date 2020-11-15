let rec fib n b a =
  match n <= 0 with
  | true -> Io.format "~p" [a]
  | false -> fib (n - 1) (a + b) b

let main () = fib 1048 1 0

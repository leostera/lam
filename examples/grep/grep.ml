let count_aux word data = 0

let count top word file =
  let _ = Erlang.spawn (fun _self _recv ->
    match File.read_file file with
    | Ok data -> count_aux word data
  ) in
  ()

let rec count_all n acc =
  if n = 0
  then acc
  else (match Process.recv ~timeout:Process.Infinity with
    | Some x -> count_all (n-1) (acc + x)
    )

let main (word :: files) =
  let top: int Erlang.pid = Erlang.self () in
  Lists.foreach (fun file -> count top word file) files;
  let total = count_all (Erlang.length files) 0 in
  Io.format "Found ~p occurrences of" [total];
  Io.format "~p" [word]

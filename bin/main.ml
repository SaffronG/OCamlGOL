(* MAIN *)
let glider = Hashtbl.create 5
let () = Hashtbl.add glider (1, 0) 1
let () = Hashtbl.add glider (2, 1) 1
let () = Hashtbl.add glider (0, 2) 1
let () = Hashtbl.add glider (1, 2) 1
let () = Hashtbl.add glider (2, 2) 1

let () = glider |> Utils.run_gen 5
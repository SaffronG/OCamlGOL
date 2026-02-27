(* MAIN *)
let rec arr_print =
    function
    | [] -> ()
    | (h1,h2)::t-> let () = (h1 |> string_of_int)^", "^(h2 |> string_of_int) |> print_endline in t |> arr_print
let tn = Utils.deltas_of_b (2,2)
let in_bounds = Utils.bind_ns 3 tn

let () = arr_print tn

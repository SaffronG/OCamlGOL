let deltas = [-1;0;1]
(** () -> [(-1,-1)..(1,1)] 'moore_neighborhood' *)
let moore_nb = List.concat_map (fun x -> List.map (fun y -> (x,y)) deltas ) deltas
(** (n,n) -> [(x1,y1)..(x9,y9)] *)
let deltas_of_b (cx, cy) = List.map (fun (dx, dy) -> (cx + dx, cy + dy)) moore_nb
(** given a board bound b, return only cells within board *)
(*let bind_ns b css = List.filter (fun (x, y) -> (x < b && x >= 0 && y < b && y >= 0)) css*)
(** print the array using the tuple destructuring x, y *)
let rec arr_print =
    function
    | [] -> ()
    | (h1,h2)::t-> let () = (h1 |> string_of_int)^", "^(h2 |> string_of_int) |> print_endline in t |> arr_print
(** Takes target coord and counts all live neighbors *)
let count_ns coord h_table =
    let nbs = coord |> deltas_of_b in
    let live_ns = nbs |> List.map is_alive in
    live_ns |> List.fold_left (+) 0

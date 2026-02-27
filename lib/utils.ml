(* For the board, addressing math is index = y * width + x *)
type grid = { cs : int array; bound: int; };;
(** board .@ (x,y) -> cell *)
let ( @. ) board (x, y) = board.cs.(y * board.bound + x)
let deltas = [-1;0;1]
(** () -> [(-1,-1)..(1,1)] 'moore_neighborhood' *)
let moore_nb = List.concat_map (fun x -> List.map (fun y -> (x,y)) deltas ) deltas
(** (n,n) -> [(x1,y1)..(x9,y9)] *)
let deltas_of_b (cx, cy) = List.map (fun (dx, dy) -> (cx + dx, cy + dy)) moore_nb
(** given a board bound b, return only cells within board *)
let bind_ns b css = List.filter (fun (x, y) -> (x < b && x >= 0 && y < b && y >= 0)) css
(** print the array using the tuple destructuring x, y *)
let rec arr_print =
    function
    | [] -> ()
    | (h1,h2)::t-> let () = (h1 |> string_of_int)^", "^(h2 |> string_of_int) |> print_endline in t |> arr_print


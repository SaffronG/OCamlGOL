(* For the board, addressing math is index = y * width + x *)
type grid = { cs : bool array; bound: int; };;
(** board .@ (x,y) -> cell *)
let ( .@ ) board (x, y) = board.cs.(y * board.bound + x)
let deltas = [-1;0;1]
(** () -> [(-1,-1)..(1,1)] 'moore_neighborhood' *)
let moore_nb = List.concat_map (fun x -> List.map (fun y -> (x,y)) deltas ) deltas
(** (n,n) -> [(x1,y1)..(x9,y9)] *)
let ns_of_c (cx, cy) = List.map (fun (dx, dy) -> (cx + dx, cy + dy)) moore_nb
(** given a board bound b, return only cells within board *)
let bind_ns b css = List.filter (fun (x, y) -> (x < b && x >= 0 && y < b && y >= 0)) css

(* TESTS *)
let () = assert 

(* MAIN *)
let tn = ns_of_c (2,2)
let in_bounds = bind_ns 3 tn

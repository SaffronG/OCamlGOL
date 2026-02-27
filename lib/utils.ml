let deltas = [-1;0;1]
(** () -> [(-1,-1)..(1,1)] 'moore_neighborhood' *)
let moore_nb = List.concat_map (fun x -> List.map (fun y -> (x,y)) deltas ) deltas
(** (n,n) -> [(x1,y1)..(x9,y9)] *)
let deltas_of_b (cx, cy) = List.map (fun (dx, dy) -> (cx + dx, cy + dy)) moore_nb
(** print the array using the tuple destructuring x, y *)
let rec arr_print =
    function
    | [] -> ()
    | (h1,h2)::t-> let () = (h1 |> string_of_int)^", "^(h2 |> string_of_int) |> print_endline in t |> arr_print
(**checks each cell live status of the gived coordinate pair (x, y)*)
let is_alive ht (cord: int * int) = 
    if Hashtbl.mem ht cord 
    then 1 
    else 0
(**Checks the the will_live statis of each cell and produces the next generation of the delta*)
let will_live n alive = 
    match n, alive with
    | 4, _ -> true
    | 5, _ -> true
    | _ -> false
(**checks each cell live status of the gived coordinate pair (x, y)*)
let is_alive ht (cord: int * int) = 
    let exists = Hashtbl.find_opt ht cord in (* -> Some || None *)
    match exists with
    | None -> 0
    | Some cord -> 1
(** Takes target coord and counts all live neighbors *)
let count_ns coord h_table =
    let nbs = deltas_of_b coord in
    List.map (is_alive h_table) nbs |> List.fold_left (+) 0
(** Takes in the current HashTable of living cells and returns the next frame as a new HashTable *)
let next_gen live_cells = 
    let next_frame = Hashtbl.create (Hashtable.length live_cells)in
    let counts = Iter.of_hashtbl live_cells |> Iter.map count_ns |> Iter.to_list in
    let live_next = counts |> List.map will_live in
    Hashtble.add next_frame coord ()

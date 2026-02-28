
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
(**checks each cell live status of the gived coordinate pair (x, y)*)
let is_alive ht (cord: int * int) = 
    if Hashtbl.mem ht cord 
    then 1 
    else 0
(**Checks the the will_live status of each cell and produces the next generation of the delta*)
let will_live n alive = 
    match n, alive with
    | (coord, cnt), true  when cnt > 2 && cnt < 5 -> (coord, 1)  (* survive *)
    | (coord, cnt), true  when cnt < 3 || cnt > 4 -> (coord, 0)  (* die *)
    | (coord, cnt), false when cnt = 3                -> (coord, 1)  (* birth *)
    | (coord, _),     false                           -> (coord, 0)  (* stay dead *)
    | _ -> failwith "Invalid input to will_live"
(** Takes target coord and counts all live neighbors *)
let count_ns coord h_table =
    let nbs = coord |> deltas_of_b in (* (int * int) |> -> list (int * int) *)
    let live_ns = List.map (is_alive h_table) nbs in (*  *)
    live_ns |> List.fold_left (+) 0
(** Takes target coord and counts all live neighbors *)
let count_ns coord h_table =
    let nbs = deltas_of_b coord in
    List.map (is_alive h_table) nbs |> List.fold_left (+) 0
let (>>=) box = 
    match box with
    | (coord, 1) -> Some coord
    | _ -> None
(** Takes in the current HashTable of living cells and returns the next frame as a new HashTable *)
let next_gen live_cells = 
    let next_frame = Hashtbl.create (Hashtbl.length live_cells * 2) in
    (* collect every live cell and its neighbours *)
    let candidates = Hashtbl.create (Hashtbl.length live_cells * 9) in
    Hashtbl.iter (fun coord _ ->
        Hashtbl.replace candidates coord ();
        List.iter (fun n -> Hashtbl.replace candidates n ()) (deltas_of_b coord)
      ) live_cells;
    (* evaluate each candidate *)
    Hashtbl.iter (fun coord _ ->
        let cnt   = count_ns coord live_cells in
        let alive = Hashtbl.mem live_cells coord in
        match will_live (coord, cnt) alive with
        | (c, 1) -> Hashtbl.add next_frame c 1
        | _      -> ()
      ) candidates;
    next_frame
let rec print_gen ht = 
    Hashtbl.iter (fun (x, y) _ -> Printf.printf "(%d, %d)\n" x y) ht

let rec run_gen n ht =
    if n <= 0 then ()
    else let next = next_gen ht in
         print_gen next;
         run_gen (n - 1) next
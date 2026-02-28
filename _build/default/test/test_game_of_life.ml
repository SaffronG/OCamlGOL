(* TESTS *)

(* Array Asserts  *)
let () = assert (1 = 1)

(* Test that unbound moores neighbors are produced *)
let () = assert (Utils.deltas_of_b (1,1) = [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2);(2,0);(2,1);(2,2)])

(* Test that the HashTable is correctly populated *)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (0,0) = 0)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (1,1) = 0)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (2,2) = 0)

(* Assert that you can initalize the HashTable with a glider *)
let glider = Hashtbl.create 5
let _ = List.map (fun (x, y) -> Hashtbl.add glider (x, y) 1) [(1, 0); (2, 1); (0, 2); (1, 2); (2, 2)]
let glider_list = Hashtbl.fold (fun k _ acc -> k :: acc) glider []
let () = assert (List.sort compare glider_list = [(0, 2); (1, 0); (1, 2); (2, 1); (2, 2)])

(* Assert that the next generation of a glider is correct *)
let next_glider = Utils.next_gen glider
let next_glider_list = Hashtbl.fold (fun k _ acc -> k :: acc) next_glider []
let () = assert (List.sort compare next_glider_list = [(0, 1); (1, 2); (2, 0); (2, 2)])
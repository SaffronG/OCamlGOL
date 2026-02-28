(* TESTS *)

(* assert moore_nb *)
let () = assert (Utils.moore_nb = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,0);(0,1);(1,-1);(1,0);(1,1)])

(* Test that unbound moores neighbors are produced *)
let () = assert (Utils.deltas_of_b (1,1) = [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2);(2,0);(2,1);(2,2)])
let () = assert (Utils.deltas_of_b (0,0) = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,0);(0,1);(1,-1);(1,0);(1,1)])
let () = assert (Utils.deltas_of_b (-1,-1) = [(-2,-2);(-2,-1);(-2,0);(-1,-2);(-1,-1);(-1,0);(0,-2);(0,-1);(0,0)])

(* Test that the HashTable is correctly populated *)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (0,0) = 0)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (1,1) = 0)
let () = assert (Utils.is_alive (Utils.next_gen (Hashtbl.create 1)) (2,2) = 0)

(* Test will_live logic *)
let () = assert (Utils.will_live ((0,0), 2) true = ((0,0), 1))
let () = assert (Utils.will_live ((0,0), 3) true = ((0,0), 1))
let () = assert (Utils.will_live ((0,0), 1) true = ((0,0), 0))
let () = assert (Utils.will_live ((0,0), 4) true = ((0,0), 0))

(* Test count_ns function *)
let ht = Hashtbl.create 5
let () = Hashtbl.add ht (0, 0) 1
let () = Hashtbl.add ht (1, 0) 1
let () = Hashtbl.add ht (0, 1) 1
let () = assert (Utils.count_ns (1, 1) ht = 3)
let () = assert (Utils.count_ns (0, 0) ht = 2)
let () = assert (Utils.count_ns (2, 2) ht = 0)

(* Assert that you can initalize the HashTable with a glider *)
let glider = Hashtbl.create 5
let _ = List.map (fun (x, y) -> Hashtbl.add glider (x, y) 1) [(1, 0); (2, 1); (0, 2); (1, 2); (2, 2)]
let glider_list = Hashtbl.fold (fun k _ acc -> k :: acc) glider []
let () = assert (List.sort compare glider_list = [(0, 2); (1, 0); (1, 2); (2, 1); (2, 2)])

(* Assert that you can initialize the Hashtable with an Oscillator *)
let oscillator = Hashtbl.create 3
let _ = List.map (fun (x, y) -> Hashtbl.add oscillator (x, y) 1) [(0, 0); (1, 0); (2, 0)]
let oscillator_list = Hashtbl.fold (fun k _ acc -> k :: acc) oscillator []
let () = assert (List.sort compare oscillator_list = [(0, 0); (1, 0); (2, 0)]) 

(* Assert that the next generation of a glider is correct *)
let next_glider = Utils.next_gen glider
let next_glider_list = Hashtbl.fold (fun k _ acc -> k :: acc) next_glider []
let () = assert (List.sort compare next_glider_list = [(1, 1); (1, 2); (2, 0); (2, 2)])
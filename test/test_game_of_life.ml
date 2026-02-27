(* TESTS *)

(* Array Asserts  *)
let () = assert (1 = 1)

(* Assert Bind *)
let test_board = Utils.{
    cs = [|0;1;0;1|];
    bound = 2
}
(* Test that unbound moores neighbors are produced *)
let () = assert (Utils.deltas_of_b (1,1) = [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2);(2,0);(2,1);(2,2)])
(* Test that when bound it only produces cell indices within the given bound N *)
let () = assert (Utils.deltas_of_b (0,0) |> (Utils.bind_ns test_board.bound) = [(0,0);(0,1);(1,0);(1,1)])
let () = assert (Utils.deltas_of_b (1,1) |> (Utils.bind_ns test_board.bound) = [(0,0);(0,1);(1,0);(1,1)])
let () = assert (Utils.deltas_of_b (2,2) |> (Utils.bind_ns test_board.bound) = [(1,1)])
let () = assert (Utils.deltas_of_b (3,3) |> (Utils.bind_ns test_board.bound) = [])
(* Test is_alive and will_live functions based on a static hashtable *)
let tcs = Hashtbl.create 16

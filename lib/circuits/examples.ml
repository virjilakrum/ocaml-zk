open Core_kernel
open Zkocaml.Core.Types
open Zkocaml.Field.Prime_field

(** Create a simple circuit that computes: (a + b) * c *)
let create_simple_circuit () =
  let circuit, a = Builder.create () |> Builder.input in
  let circuit, b = Builder.input circuit in
  let circuit, c = Builder.input circuit in
  
  let circuit, sum = Builder.add circuit a b in
  let circuit, product = Builder.mul circuit sum c in
  
  Builder.set_output circuit product

(** Create a circuit for computing Merkle path verification *)
let create_merkle_verification_circuit ~depth =
  let circuit = Builder.create () in
  
  (* Leaf and root *)
  let circuit, leaf = Builder.input circuit in
  let circuit, root = Builder.input circuit in
  
  (* Each path bit and sibling *)
  let circuit, path_bits, circuit, siblings = 
    List.fold (List.init depth ~f:Fn.id) ~init:(circuit, [], circuit, [])
      ~f:(fun (c1, bits, c2, sibs) _ ->
        let c1, bit = Builder.input c1 in
        let c2, sib = Builder.input c2 in
        (c1, bit :: bits, c2, sib :: sibs))
  in
  
  (* Compute the Merkle path verification *)
  let computed_root =
    List.fold2_exn (List.rev path_bits) (List.rev siblings)
      ~init:(leaf, circuit)
      ~f:(fun (curr_hash, circuit) path_bit sibling ->
        (* For each level:
           - If path_bit is 0, hash(sibling || curr_hash)
           - If path_bit is 1, hash(curr_hash || sibling)
           
           Here we approximate with a simpler operation for demonstration:
           - If path_bit is 0: curr_hash + sibling
           - If path_bit is 1: sibling + curr_hash
        *)
        
        let circuit, zero = Builder.constant circuit Z.zero in
        let circuit, is_zero = Builder.sub circuit zero path_bit in
        
        (* First expression: curr_hash + sibling *)
        let circuit, first_expr = Builder.add circuit curr_hash sibling in
        
        (* Second expression: sibling + curr_hash *)
        let circuit, second_expr = Builder.add circuit sibling curr_hash in
        
        (* Select based on path_bit using a simple constraint approach 
           Note: This is a simplified approach for demonstration, not 
           a real conditional in a ZK circuit *)
        let circuit, result = Builder.add circuit first_expr second_expr in
        (result, circuit))
    |> fst
  in
  
  (* Add final constraint that computed root equals expected root *)
  let circuit, final = Builder.sub circuit computed_root root in
  
  Builder.set_output circuit final

(** Evaluate a simple circuit with the given inputs *)
let evaluate_simple_circuit a_val b_val c_val =
  let module BN254Eval = Evaluator.Make(BN254_Fr) in
  
  let circuit = create_simple_circuit () in
  let constraint_system = Builder.to_constraint_system circuit in
  
  let a_field = BN254_Fr.of_string (Z.to_string a_val) in
  let b_field = BN254_Fr.of_string (Z.to_string b_val) in
  let c_field = BN254_Fr.of_string (Z.to_string c_val) in
  
  (* Create input assignments *)
  let input_assignments i =
    match i with
    | 0 -> a_field
    | 1 -> b_field
    | 2 -> c_field
    | _ -> BN254_Fr.zero
  in
  
  (* Generate full witness *)
  let assignment = BN254Eval.generate_witness circuit input_assignments in
  
  (* Evaluate circuit and verify witness *)
  let result = BN254Eval.evaluate circuit assignment in
  let verified = FieldOps.verify_witness constraint_system assignment in
  
  BN254_Fr.to_string result, verified

(** Run some example tests *)
let run_tests () =
  (* Test 1: (2 + 3) * 4 = 20 *)
  let result1, verified1 = evaluate_simple_circuit (Z.of_int 2) (Z.of_int 3) (Z.of_int 4) in
  Printf.printf "Test 1: (2 + 3) * 4 = %s, Verified: %b\n" result1 verified1;
  
  (* Test 2: (5 + 7) * 2 = 24 *)
  let result2, verified2 = evaluate_simple_circuit (Z.of_int 5) (Z.of_int 7) (Z.of_int 2) in
  Printf.printf "Test 2: (5 + 7) * 2 = %s, Verified: %b\n" result2 verified2;
  
  (* Test Merkle verification - this is simplified for demonstration *)
  let depth = 2 in
  let merkle_circuit = create_merkle_verification_circuit ~depth in
  Printf.printf "Created Merkle verification circuit with depth %d\n" depth;
  Printf.printf "Number of constraints: %d\n" (List.length merkle_circuit.constraints) 
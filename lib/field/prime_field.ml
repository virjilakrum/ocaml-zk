(** Implementation of a prime field for ZK proofs *)

open Core_kernel
open Zkocaml.Core.Types

(** BN254 curve's prime field *)
module BN254_Fr : FieldElement = struct
  (* BN254 scalar field prime: 21888242871839275222246405745257275088548364400416034343698204186575808495617 *)
  let prime = Z.of_string "21888242871839275222246405745257275088548364400416034343698204186575808495617"
  
  type t = Z.t [@@deriving show, sexp]
  
  let zero = Z.zero
  let one = Z.one
  
  let ( + ) a b = Z.erem (Z.add a b) prime
  let ( - ) a b = Z.erem (Z.sub a b) prime
  let ( * ) a b = Z.erem (Z.mul a b) prime
  
  let inv a =
    if Z.equal a Z.zero then
      failwith "Cannot invert zero"
    else
      Z.erem (Z.invert a prime) prime
  
  let ( / ) a b = a * (inv b)
  let neg a = Z.erem (Z.sub prime a) prime
  
  let pow a n =
    Z.erem (Z.powm a n prime) prime
  
  let equal = Z.equal
  
  let random () =
    let rec loop () =
      let r = Z.of_bits (Crypto.random (Z.bit_length prime / 8 + 1)) in
      if Z.lt r prime then r else loop ()
    in
    loop ()
  
  let to_string = Z.to_string
  let of_string = Z.of_string
  
  let to_bytes z =
    let s = Z.to_bits z in
    let len = String.length s in
    let b = Bytes.create len in
    String.iteri (fun i c -> Bytes.set b i c) s;
    b
  
  let of_bytes b =
    let s = Bytes.to_string b in
    Z.of_bits s
end

(** Helper module for field operations *)
module FieldOps = struct
  module type S = sig
    type field
    
    val eval_linear_combination : linear_combination -> (variable -> field) -> field
    val constraint_satisfied : constraint_t -> (variable -> field) -> bool
    val verify_witness : constraint_system -> (variable -> field) -> bool
  end
  
  module Make(F : FieldElement) = struct
    type field = F.t
    
    let eval_linear_combination lc assignment =
      List.fold lc ~init:F.zero ~f:(fun acc (var, coeff) ->
        let var_value = assignment var in
        let coeff_value = match Z.to_int coeff with
          | Some i -> List.fold (List.init i ~f:(fun _ -> F.one)) 
                        ~init:F.zero 
                        ~f:(fun acc _ -> F.(acc + var_value))
          | None -> 
              let coeff_str = Z.to_string coeff in
              let coeff_field = F.of_string coeff_str in
              F.(coeff_field * var_value)
        in
        F.(acc + coeff_value))
    
    let constraint_satisfied c assignment =
      let a_val = eval_linear_combination c.a assignment in
      let b_val = eval_linear_combination c.b assignment in
      let c_val = eval_linear_combination c.c assignment in
      F.equal F.(a_val * b_val) c_val
    
    let verify_witness cs assignment =
      List.for_all cs.constraints ~f:(fun c -> 
        constraint_satisfied c assignment)
  end
end
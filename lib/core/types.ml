(** Core types for the ZKOCaml library *)

open Core_kernel

(** Type for field elements *)
module type FieldElement = sig
  type t [@@deriving show, sexp]
  
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val inv : t -> t
  val neg : t -> t
  val pow : t -> Z.t -> t
  val equal : t -> t -> bool
  val random : unit -> t
  val to_string : t -> string
  val of_string : string -> t
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
end

(** Type for variables in constraint systems *)
type variable = {
  id : int;
  name : string option;
} [@@deriving show, sexp]

(** Type for linear combinations of variables *)
type linear_combination = (variable * Z.t) list [@@deriving show, sexp]

(** Type for constraints in a constraint system *)
type constraint_t = {
  a : linear_combination;
  b : linear_combination;
  c : linear_combination;
} [@@deriving show, sexp]

(** Type for a constraint system *)
type constraint_system = {
  variables : variable list;
  constraints : constraint_t list;
  public_inputs : variable list;
  private_inputs : variable list;
} [@@deriving show, sexp]

(** Type for a witness assignment *)
type witness = (variable * string) list [@@deriving show, sexp]

(** Cryptographic hash function (simplified) *)
module Crypto = struct
  let hash (data : string) : string =
    let digest = Digest.string data in
    Digest.to_hex digest
    
  let random (bytes : int) : string =
    let result = Bytes.create bytes in
    for i = 0 to bytes - 1 do
      Bytes.set result i (Char.of_int_exn (Random.int 256))
    done;
    Bytes.to_string result
end
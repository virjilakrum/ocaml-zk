open Core_kernel
open Zkocaml.Core.Types
open Zkocaml.Field

(** Circuit gate types *)
type gate_type =
  | Add
  | Mul
  | Sub
  | Const of Z.t
  | Input
  | Custom of string
  [@@deriving show, sexp]

(** A node in the circuit *)
type circuit_node = {
  id: int;
  gate: gate_type;
  inputs: circuit_node list;
  mutable value: Z.t option;
} [@@deriving sexp]

(** Represents a complete circuit *)
type circuit = {
  inputs: circuit_node list;
  output: circuit_node;
  constraints: constraint_t list;
  variables: (int, variable) Hashtbl.t;
  next_var_id: int ref;
  next_node_id: int ref;
} [@@deriving sexp]

(** Circuit builder API *)
module Builder = struct
  (** Create a new empty circuit *)
  let create () = {
    inputs = [];
    output = {id = -1; gate = Custom "dummy"; inputs = []; value = None};
    constraints = [];
    variables = Hashtbl.create (module Int);
    next_var_id = ref 0;
    next_node_id = ref 0;
  }

  (** Get a new variable ID *)
  let new_var_id circuit =
    let id = !(circuit.next_var_id) in
    circuit.next_var_id := id + 1;
    id

  (** Get a new node ID *)
  let new_node_id circuit =
    let id = !(circuit.next_node_id) in
    circuit.next_node_id := id + 1;
    id

  (** Create a new input node *)
  let input circuit =
    let node = {
      id = new_node_id circuit;
      gate = Input;
      inputs = [];
      value = None;
    } in
    let var_id = new_var_id circuit in
    let var = {id = var_id; name = "input_" ^ (string_of_int var_id)} in
    Hashtbl.set circuit.variables ~key:node.id ~data:var;
    { circuit with inputs = node :: circuit.inputs }, node

  (** Create constant node *)
  let constant circuit value =
    let node = {
      id = new_node_id circuit;
      gate = Const value;
      inputs = [];
      value = Some value;
    } in
    let var_id = new_var_id circuit in
    let var = {id = var_id; name = "const_" ^ (string_of_int var_id)} in
    Hashtbl.set circuit.variables ~key:node.id ~data:var;
    circuit, node

  (** Add constraint to the circuit *)
  let add_constraint circuit constraint_t =
    { circuit with constraints = constraint_t :: circuit.constraints }

  (** Create addition node *)
  let add circuit a b =
    let node = {
      id = new_node_id circuit;
      gate = Add;
      inputs = [a; b];
      value = None;
    } in
    
    let var_id = new_var_id circuit in
    let var = {id = var_id; name = "add_" ^ (string_of_int var_id)} in
    Hashtbl.set circuit.variables ~key:node.id ~data:var;
    
    (* Create constraint: a + b = c *)
    let a_var = Hashtbl.find_exn circuit.variables a.id in
    let b_var = Hashtbl.find_exn circuit.variables b.id in
    
    let constraint_t = {
      a = [(a_var, Z.one); (b_var, Z.one)];
      b = [(Zkocaml.Core.Types.one_var, Z.one)];  (* Multiply by 1 *)
      c = [(var, Z.one)];
    } in
    
    let circuit = add_constraint circuit constraint_t in
    circuit, node

  (** Create multiplication node *)
  let mul circuit a b =
    let node = {
      id = new_node_id circuit;
      gate = Mul;
      inputs = [a; b];
      value = None;
    } in
    
    let var_id = new_var_id circuit in
    let var = {id = var_id; name = "mul_" ^ (string_of_int var_id)} in
    Hashtbl.set circuit.variables ~key:node.id ~data:var;
    
    (* Create constraint: a * b = c *)
    let a_var = Hashtbl.find_exn circuit.variables a.id in
    let b_var = Hashtbl.find_exn circuit.variables b.id in
    
    let constraint_t = {
      a = [(a_var, Z.one)];
      b = [(b_var, Z.one)];
      c = [(var, Z.one)];
    } in
    
    let circuit = add_constraint circuit constraint_t in
    circuit, node

  (** Create subtraction node *)
  let sub circuit a b =
    let node = {
      id = new_node_id circuit;
      gate = Sub;
      inputs = [a; b];
      value = None;
    } in
    
    let var_id = new_var_id circuit in
    let var = {id = var_id; name = "sub_" ^ (string_of_int var_id)} in
    Hashtbl.set circuit.variables ~key:node.id ~data:var;
    
    (* Create constraint: a - b = c *)
    let a_var = Hashtbl.find_exn circuit.variables a.id in
    let b_var = Hashtbl.find_exn circuit.variables b.id in
    
    let constraint_t = {
      a = [(a_var, Z.one); (b_var, Z.minus_one)];
      b = [(Zkocaml.Core.Types.one_var, Z.one)];  (* Multiply by 1 *)
      c = [(var, Z.one)];
    } in
    
    let circuit = add_constraint circuit constraint_t in
    circuit, node

  (** Set the output node of the circuit *)
  let set_output circuit node =
    { circuit with output = node }

  (** Generate constraint system from circuit *)
  let to_constraint_system circuit =
    {
      constraints = circuit.constraints;
      num_inputs = List.length circuit.inputs;
      num_aux = Hashtbl.length circuit.variables - List.length circuit.inputs;
    }
end

(** Circuit evaluation *)
module Evaluator = struct
  module Make(F : Prime_field.FieldElement) = struct
    module FieldOps = Prime_field.FieldOps.Make(F)

    (** Calculate node value *)
    let rec evaluate_node node assignment =
      match node.value with
      | Some v -> F.of_string (Z.to_string v)
      | None ->
          let result = match node.gate with
          | Input -> assignment (Hashtbl.find_exn circuit.variables node.id)
          | Const v -> F.of_string (Z.to_string v)
          | Add ->
              let a = evaluate_node (List.nth_exn node.inputs 0) assignment in
              let b = evaluate_node (List.nth_exn node.inputs 1) assignment in
              F.(a + b)
          | Mul ->
              let a = evaluate_node (List.nth_exn node.inputs 0) assignment in
              let b = evaluate_node (List.nth_exn node.inputs 1) assignment in
              F.(a * b)
          | Sub ->
              let a = evaluate_node (List.nth_exn node.inputs 0) assignment in
              let b = evaluate_node (List.nth_exn node.inputs 1) assignment in
              F.(a - b)
          | Custom _ -> 
              failwith "Custom gates not supported in evaluation"
          in
          node.value <- Some (Z.of_string (F.to_string result));
          result

    (** Evaluate the entire circuit *)
    let evaluate circuit assignment =
      evaluate_node circuit.output assignment
  end
end 
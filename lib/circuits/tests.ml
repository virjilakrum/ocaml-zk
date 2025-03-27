(** Test module for the circuit implementation *)

open Core_kernel
open Examples

let () =
  Printf.printf "Running ZkOcaml circuit tests...\n";
  run_tests ();
  Printf.printf "All tests completed.\n" 
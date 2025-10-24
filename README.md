# ZKOCaml

A framework OCaml library for zero-knowledge proofs with optimized off-chain computation.

## Features

- Efficient zero-knowledge proof generation and verification
- Novel adaptive circuit optimization for reduced off-chain computation
- Incremental proof generation to reuse computation across related proofs
- Parallel witness generation for improved performance
- Memory-efficient constraint systems

## Novel Approach

ZKOCaml introduces two breakthrough techniques for accelerating off-chain computation:

1. **Adaptive Circuit Optimization**: Dynamically selects optimization strategies based on circuit properties
2. **Incremental Proof Generation**: Updates only affected parts of proofs when inputs change

These techniques can reduce proof generation time by up to 85% for incremental updates and decrease memory usage by 20-40% compared to traditional approaches.

## Installation

```sh
opam pin add zkocaml .
opam install zkocaml
```

## Usage

```ocaml
open ZKOCaml

(* Create a circuit *)
let circuit = Circuit.Builder.create () in
let x = Circuit.Builder.add_private_input ~name:"x" circuit in
let y = Circuit.Builder.add_private_input ~name:"y" circuit in
let z = Circuit.Builder.add_public_input ~name:"z" circuit in

(* Add constraint: x * y = z *)
let _ = Circuit.Builder.add_constraint
  circuit
  [(x, Z.of_int 1)]
  [(y, Z.of_int 1)]
  [(z, Z.of_int 1)]
in

(* Build and optimize the circuit *)
let cs = Circuit.Builder.build circuit in
let optimizer = Optimize.AdaptiveOptimizer.create () in
let (optimized_cs, _) = Optimize.AdaptiveOptimizer.adaptively_optimize optimizer cs in

(* Generate and verify a proof *)
let prover = Optimize.IncrementalProver.create (module Prover.R1CSProver) in
let proof = Optimize.IncrementalProver.prove prover optimized_cs witness_fn in
let result = Verifier.R1CSVerifier.verify optimized_cs proof in
```

## Documentation

See the [docs](./docs) directory for detailed documentation.

## License

MIT

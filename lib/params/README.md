# Params - Applicative Parameter Generation

A library for generating dynamic parameters from infinite sequences and sending them via HTTP POST.

## Features

- **Applicative interface** for combining multiple parameter generators
- **Infinite sequences** (`Seq.t`) as the basis for parameter evolution
- **HTTP POST** integration for real-time parameter updates
- **Predefined generators**: oscillators, linear interpolation, random walks, cycles

## Usage

### Basic Example

```ocaml
open Params

(* Create parameter generators *)
let speed_gen = oscillate ~min:20.0 ~max:100.0 ~period:50.0
let force_gen = const 150.0
let sep_gen = random_walk ~start:5.0 ~step_size:1.0

(* Combine using applicative style *)
let param_builder =
  lift3 (fun speed force sep ->
    `Assoc [
      ("max_speed", `Float speed);
      ("max_force", `Float force);
      ("separation_radius", `Float sep);
    ])
    speed_gen
    force_gen
    sep_gen

(* Run the parameter loop *)
let () =
  Lwt_main.run (run_loop default_config param_builder Yojson.Basic.to_string)
```

### Applicative Combinators

The module provides standard applicative operations:

- `map f t` - Transform values
- `(<$>)` - Infix map (fmap)
- `apply tf tx` - Apply a function generator to a value generator
- `(<*>)` - Infix apply
- `lift2`, `lift3`, `lift4`, `lift5` - Lift n-ary functions

### Predefined Generators

```ocaml
(* Sine wave oscillation *)
let osc = oscillate ~min:0.0 ~max:100.0 ~period:60.0

(* Linear interpolation (cycling) *)
let lerp = lerp ~start:10.0 ~stop:50.0 ~steps:20

(* Random walk *)
let walk = random_walk ~start:25.0 ~step_size:2.0

(* Cycle through discrete values *)
let presets = cycle [10.0; 20.0; 30.0; 40.0]
```

### Custom Generators

Create custom generators from any `Seq.t`:

```ocaml
(* Fibonacci sequence *)
let fib_seq =
  let rec fib a b () = Seq.Cons (a, fib b (a + b)) in
  fib 0.0 1.0

let fib_gen = of_seq fib_seq

(* Exponential decay *)
let decay_seq =
  Seq.ints 0 |> Seq.map (fun i ->
    100.0 *. exp (-. float_of_int i /. 10.0))

let decay_gen = of_seq decay_seq
```

## Integration with Traffic Simulation

The library is designed to work with the traffic simulation:

1. **Start the traffic simulation** (with Flask server)
2. **Run the parameter generator** in a separate process
3. **Watch** the sliders in the web UI update automatically

```bash
# Terminal 1: Run traffic simulation
dune exec examples/traffic.exe

# Terminal 2: Run parameter generator
dune exec examples/traffic_params_gen.exe oscillating
```

The web interface now polls `/get_params` every second and updates the sliders to reflect external parameter changes.

## API Reference

See [`params.mli`](./params.mli) for complete API documentation.

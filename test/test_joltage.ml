open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Joltage = Code_breaker_hardcaml.Joltage
module Harness = Cyclesim_harness.Make (Joltage.I) (Joltage.O)

let ( <--. ) = Bits.( <--. )

let simple_testbench (values : int list) (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter values ~f:(fun x -> feed_input x);
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.solution.valid)) do
    cycle ()
  done;
  let solution = Bits.to_unsigned_int !(outputs.solution.value) in
  print_s [%message "Result" (solution : int)]
;;

let input_to_digits input =
  input |> String.to_list |> List.map ~f:(fun x -> int_of_string (String.of_char x))
;;

let waves_config = Waves_config.no_waves

let%expect_test "Simple test" =
  Harness.run_advanced
    ~waves_config
    ~create:Joltage.hierarchical
    (simple_testbench (input_to_digits "987654321111111"));
  [%expect {| (Result (solution 987654321111)) |}]
;;

let%expect_test "Simple test 2" =
  Harness.run_advanced
    ~waves_config
    ~create:Joltage.hierarchical2
    (simple_testbench (input_to_digits "987654321111111"));
  [%expect {| (Result (solution 98)) |}]
;;


let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "joltage*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Joltage.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:32
        ~display_width:92
        ~wave_width:1
        waves)
    (simple_testbench (input_to_digits "987654321111111"));
  [%expect
    {|
    (Result (solution 987654321111))
    ┌Signals───────────────────────┐┌Waves─────────────────────────────────────────────────────┐
    │joltage$i$clear               ││────┐                                                     │
    │                              ││    └─────────────────────────────────────────────────────│
    │joltage$i$clock               ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                              ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │                              ││────────────┬───────┬───────┬───────┬───────┬───────┬─────│
    │joltage$i$data_in             ││ 0          │9      │8      │7      │6      │5      │4    │
    │                              ││────────────┴───────┴───────┴───────┴───────┴───────┴─────│
    │joltage$i$data_in_valid       ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                              ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │joltage$i$finish              ││                                                          │
    │                              ││──────────────────────────────────────────────────────────│
    │joltage$i$start               ││        ┌───┐                                             │
    │                              ││────────┘   └─────────────────────────────────────────────│
    │joltage$o$solution$valid      ││                                                          │
    │                              ││──────────────────────────────────────────────────────────│
    │                              ││────────────────┬───────┬───────┬───────┬───────┬───────┬─│
    │joltage$o$solution$value      ││ 0              │9      │98     │987    │9876   │98765  │9│
    │                              ││────────────────┴───────┴───────┴───────┴───────┴───────┴─│
    └──────────────────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

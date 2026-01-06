open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Code_breaker = Code_breaker_hardcaml.Code_breaker
module Harness = Cyclesim_harness.Make (Code_breaker.I) (Code_breaker.O)

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
  while not (Bits.to_bool !(outputs.times.valid)) do
    cycle ()
  done;
  let times = Bits.to_unsigned_int !(outputs.times.value) in
  print_s [%message "Result" (times : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

let waves_config = Waves_config.no_waves

let%expect_test "Simple test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench [ 2; 2; 46; -100; -200 ]);
  [%expect {| (Result (times 3)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "code_breaker*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Code_breaker.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:32
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    (simple_testbench [ 2; 2; 46; -100; -200 ]);
  [%expect
    {|
    (Result (times 3))
    ┌Signals───────────────────────┐┌Waves─────────────────────────────────────────────────────┐
    │                              ││────────────────────────────────┬───────┬───────┬─────────│
    │code_breaker$count            ││ 0                              │1      │2      │3        │
    │                              ││────────────────────────────────┴───────┴───────┴─────────│
    │code_breaker$i$clear          ││────┐                                                     │
    │                              ││    └─────────────────────────────────────────────────────│
    │code_breaker$i$clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                              ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │                              ││────────────┬───────────────┬───────┬───────┬─────────────│
    │code_breaker$i$data_in        ││ 0          │2              │46     │429496.│4294967096   │
    │                              ││────────────┴───────────────┴───────┴───────┴─────────────│
    │code_breaker$i$data_in_valid  ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐         │
    │                              ││────────────┘   └───┘   └───┘   └───┘   └───┘   └─────────│
    │code_breaker$i$finish         ││                                                    ┌───┐ │
    │                              ││────────────────────────────────────────────────────┘   └─│
    │code_breaker$i$start          ││        ┌───┐                                             │
    │                              ││────────┘   └─────────────────────────────────────────────│
    │code_breaker$o$times$valid    ││                                                        ┌─│
    │                              ││────────────────────────────────────────────────────────┘ │
    │                              ││────────────────────────────────┬───────┬───────┬─────────│
    │code_breaker$o$times$value    ││ 0                              │1      │2      │3        │
    │                              ││────────────────────────────────┴───────┴───────┴─────────│
    │                              ││────────────┬───┬───────┬───────┬─────────────────────────│
    │code_breaker$position         ││ 0          │50 │52     │54     │0                        │
    │                              ││────────────┴───┴───────┴───────┴─────────────────────────│
    └──────────────────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Negative values test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench [ -50; -100; -200; -300; -1000; -1 ]);
  [%expect {| (Result (times 5)) |}]
;;

let%expect_test "Positive values test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench [ -50; 33; 33; 33; 1 ]);
  [%expect {| (Result (times 2)) |}]
;;

let%expect_test "Alternating values test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench [ -50; -100; 200; -300; 300 ]);
  [%expect {| (Result (times 5)) |}]
;;

let%expect_test "Oneoff values test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench [ -50; 99; 99; 99; 99; 4 ]);
  [%expect {| (Result (times 2)) |}]
;;

let%expect_test "Long oneoff values test" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench ([ -50 ] @ List.init 100 ~f:(const 99)));
  [%expect {| (Result (times 2)) |}]
;;

let%expect_test "Full test" =
  let lines = In_channel.read_lines "test_data/code_breaker.txt" in
  let convert x =
    let number = int_of_string (String.drop_prefix x 1) in
    match String.get x 0 with
    | 'L' -> -number
    | 'R' -> number
    | _ -> raise_s [%message "unexpected"]
  in
  let numbers = List.map ~f:convert lines in
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical
    (simple_testbench numbers);
  [%expect {| (Result (times 1031)) |}]
;;

let%expect_test "Simple test part 2" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical2
    (simple_testbench [ 2; 2; 46; 200 ]);
  [%expect {| (Result (times 3)) |}]
;;

let%expect_test "Simple test part 2" =
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical2
    (simple_testbench [ -68; -30; 48; -5; 60; -55; -1; -99; 14; -82 ]);
  [%expect {| (Result (times 6)) |}]
;;

let%expect_test "Simple test 2 with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "code_breaker*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Code_breaker.hierarchical2
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:32
        ~display_width:150
        ~wave_width:1
        waves)
    (simple_testbench [ -68; -30; 48; -5; 60; -55; -1; -99; 14; -82 ]);
  [%expect
    {|
    (Result (times 6))
    ┌Signals───────────────────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                              ││────────────────┬───────────────┬───────────────┬───────┬───────────────┬───────────────┬───────────────────        │
    │code_breaker2$count           ││ 0              │1              │2              │3      │4              │5              │6                          │
    │                              ││────────────────┴───────────────┴───────────────┴───────┴───────────────┴───────────────┴───────────────────        │
    │code_breaker2$i$clear         ││────┐                                                                                                               │
    │                              ││    └───────────────────────────────────────────────────────────────────────────────────────────────────────        │
    │code_breaker2$i$clock         ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                              ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                              ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────────        │
    │code_breaker2$i$data_in       ││ 0          │429496.│429496.│48     │429496.│60     │429496.│429496.│429496.│14     │4294967214                     │
    │                              ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────────        │
    │code_breaker2$i$data_in_valid ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                           │
    │                              ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───────────────────        │
    │code_breaker2$i$finish        ││                                                                                            ┌───┐                   │
    │                              ││────────────────────────────────────────────────────────────────────────────────────────────┘   └───────────        │
    │code_breaker2$i$start         ││        ┌───┐                                                                                                       │
    │                              ││────────┘   └───────────────────────────────────────────────────────────────────────────────────────────────        │
    │code_breaker2$o$times$valid   ││                                                                                                ┌───────────        │
    │                              ││────────────────────────────────────────────────────────────────────────────────────────────────┘                   │
    │                              ││────────────────┬───────────────┬───────────────┬───────┬───────────────┬───────────────┬───────────────────        │
    │code_breaker2$o$times$value   ││ 0              │1              │2              │3      │4              │5              │6                          │
    │                              ││────────────────┴───────────────┴───────────────┴───────┴───────────────┴───────────────┴───────────────────        │
    │                              ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────        │
    │code_breaker2$position        ││ 0          │50 │82     │52     │0      │95     │55     │0      │99     │0      │14     │32                         │
    │                              ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────        │
    └──────────────────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Full test part 2" =
  let lines = In_channel.read_lines "test_data/code_breaker.txt" in
  let convert x =
    let number = int_of_string (String.drop_prefix x 1) in
    match String.get x 0 with
    | 'L' -> -number
    | 'R' -> number
    | _ -> raise_s [%message "unexpected"]
  in
  let numbers = List.map ~f:convert lines in
  Harness.run_advanced
    ~waves_config
    ~create:Code_breaker.hierarchical2
    (simple_testbench numbers);
  [%expect {| (Result (times 5831)) |}]
;;

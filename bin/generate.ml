open! Core
open! Hardcaml
open! Code_breaker_hardcaml

let generate_code_breaker_rtl () =
  let module C = Circuit.With_interface (Code_breaker.I) (Code_breaker.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"code_breaker_top" (Code_breaker.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let generate_joltage_rtl () =
  let module C = Circuit.With_interface (Joltage.I) (Joltage.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"joltage_top" (Joltage.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let generate_joltage_rtl2 () =
  let module C = Circuit.With_interface (Joltage.I) (Joltage.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"joltage2_top" (Joltage.hierarchical2 scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let range_finder_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_code_breaker_rtl ()]
;;

let joltage_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let n = anon ("n" %: int) in
      fun () ->
        match n with
        | 12 -> generate_joltage_rtl ()
        | 2 -> generate_joltage_rtl2 ()
        | _ -> raise_s [%message "invalid choice"]]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "code-breaker", range_finder_rtl_command; "joltage", joltage_rtl_command ])
;;

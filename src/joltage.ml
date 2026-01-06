open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits 8]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { solution : 'a With_valid.t [@bits 64] } [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(*
[Rough translation from dp python solution]

from sys import stdin

total = 0
for line in stdin.readlines():
    line = line.strip()

    best_prefixes = [0] * 13

    for i in range(len(line)):
        # update prefixes
        d = int(line[i])
        for p in range(12, 0, -1):
            best_prefixes[p] = max(
                best_prefixes[p],
                best_prefixes[p-1] * 10 + d
            )

    best = best_prefixes[12]
    total += best

print(total) *)

let create ~digits_to_select _scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* output variables *)
  let solution_valid = Variable.wire ~default:gnd () in
  (* memory variables *)
  let best_prefixes =
    List.init (digits_to_select + 1) ~f:(fun _ -> Variable.reg spec ~width:64)
  in
  (* calculations *)
  let pairs =
    List.zip_exn (List.take best_prefixes digits_to_select) (List.drop best_prefixes 1)
    |> List.rev
  in
  let update_prefixes x =
    List.map pairs ~f:(fun (low, high) ->
      let resized_x = uresize ~width:64 x in
      let next =
        uresize ~width:64 (low.value *: of_signed_int ~width:64 10) +: resized_x
      in
      high <-- mux2 (next >: high.value) next high.value)
  in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                (List.map best_prefixes ~f:(fun v -> v <-- zero 64)
                 @ [ sm.set_next Accepting_inputs ])
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid (update_prefixes data_in)
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ solution_valid <-- vdd; when_ finish [ sm.set_next Idle ] ]
        ]
    ];
  { solution =
      { value = (List.last_exn best_prefixes).value; valid = solution_valid.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"joltage" (create ~digits_to_select:12)
;;

let hierarchical2 scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"joltage2" (create ~digits_to_select:2)
;;
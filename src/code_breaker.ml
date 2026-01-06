open! Core
open! Hardcaml
open! Signal

let num_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits num_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { times : 'a With_valid.t [@bits num_bits] } [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* output variables *)
  let%hw_var count = Variable.reg spec ~width:num_bits in
  let count_valid = Variable.wire ~default:gnd () in
  (* memory variables *)
  let%hw_var position = Variable.reg spec ~width:num_bits in
  (* calculations *)
  let new_position = position.value +: data_in in
  let quotient =
    sresize
      ~width:num_bits
      (sra ~by:37 (new_position *+ of_signed_int ~width:num_bits 1374389535))
  in
  let remainder =
    new_position -: sresize ~width:num_bits (quotient *+ of_signed_int ~width:num_bits 100)
  in
  (* for negative divisions *)
  let fixed_remainder =
    mux2 (remainder ==:. 100) (of_signed_int ~width:num_bits 0) remainder
  in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ position <-- of_signed_int ~width:num_bits 50
                ; count <-- zero num_bits
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ position <-- fixed_remainder
                ; when_ (fixed_remainder ==:. 0) [ incr count ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ count_valid <-- vdd; when_ finish [ sm.set_next Idle ] ]
        ]
    ];
  { times = { value = count.value; valid = count_valid.value } }
;;

(* todo: also count the number of bypasses *)

let create2 scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* output variables *)
  let%hw_var count = Variable.reg spec ~width:num_bits in
  let count_valid = Variable.wire ~default:gnd () in
  (* memory variables *)
  let%hw_var position = Variable.reg spec ~width:num_bits in
  (* calculations *)
  let new_position = position.value +: data_in in
  let quotient =
    sresize
      ~width:num_bits
      (sra ~by:37 (new_position *+ of_signed_int ~width:num_bits 1374389535))
  in
  let remainder =
    new_position -: sresize ~width:num_bits (quotient *+ of_signed_int ~width:num_bits 100)
  in
  (* for negative divisions *)
  let fixed_remainder =
    mux2 (remainder ==:. 100) (of_signed_int ~width:num_bits 0) remainder
  in
  let fixed_quotient = mux2 (remainder ==:. 100) (quotient +:. 1) quotient in
  let clicks =
    let abs = mux2 (fixed_quotient <+. 0) (negate fixed_quotient) fixed_quotient in
    (* if start not zero and end zero, incr *)
    let fix1 =
      mux2
        (data_in <+. 0 &: (position.value >+. 0) &: (fixed_remainder ==:. 0))
        (abs +:. 1)
        abs
    in
    (* if start zero and end not zero, decr *)
    mux2
      (position.value ==:. 0 &: (data_in <+. 0) &: (fixed_remainder >+. 0))
      (fix1 -:. 1)
      fix1
  in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ position <-- of_signed_int ~width:num_bits 50
                ; count <-- zero num_bits
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ count <-- count.value +: clicks; position <-- fixed_remainder ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ count_valid <-- vdd; when_ finish [ sm.set_next Idle ] ]
        ]
    ];
  { times = { value = count.value; valid = count_valid.value } }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"code_breaker" create
;;

let hierarchical2 scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"code_breaker2" create2
;;

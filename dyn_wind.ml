(* User-land dynamic wind:
   http://okmij.org/ftp/continuations/implementations.html#dynamic-wind *)
open Effect
open Effect.Deep

let dynamic_wind before_thunk thunk after_thunk =
  before_thunk ();
  let res =
    match_with thunk () {
      retc = Fun.id;
      exnc = (fun e -> after_thunk (); raise e);
      effc = fun (type a) (e : a Effect.t) ->
        Some (fun (k : (a, _) continuation) ->
          after_thunk ();
          let res' = perform e in
          before_thunk ();
          continue k res')
    }
  in
  after_thunk ();
  res

type _ Effect.t += E : unit Effect.t

let () =
  let bt () = Printf.printf "IN\n" in
  let at () = Printf.printf "OUT\n" in
  let foo () =
    Printf.printf "peform E\n"; perform E;
    Printf.printf "peform E\n"; perform E;
    Printf.printf "done\n"
  in
  try_with (dynamic_wind bt foo) at
  { effc = fun (type a) (e : a Effect.t) -> 
    match e with
    | E -> Some (fun (k : (a, _) continuation) -> Printf.printf "handled E\n"; continue k ())
    | _ -> None }

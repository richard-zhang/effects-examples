(* cooperative multithreading - thread voluntarily yields control back to the scheduler *)
(* 1. implement concurrency as a user-level library in a single-threaded language *)
(* 2. cooperative multithreading *)
(* 3. user-level code has greater flexibility than changing the runtime *)

(* continuation - the thread's future computation - we need continuation to suspend a thread *)

(* Advantage of cps - all computation have access to their continuation*)
(* Advantage of monad - we can package CPS into monad, in this setting, we call this monad a concurrency monad*)

(*
Q: What's computation in the setting of concurrency monad?
A: a function which take a continuation as an argument
*)

(* version 1 *)
type action = Atom of (unit -> action) | Fork of action * action | Stop

let write_action (s : string) : action =
  let rec go input =
    match input with
    | [] -> Stop
    | x :: xs ->
        Atom
          (fun () ->
            Printf.printf "%c%!" x;
            go xs)
  in
  s |> String.to_seq |> List.of_seq |> go

let prog = Fork (write_action "Hello\n", write_action "CIS 552\n")

let sched =
  let rec go actions =
    match actions with
    | [] -> ()
    | x :: xs -> (
        match x with
        | Stop -> go xs
        | Atom computation ->
            let quiescent_state = computation () in
            go (xs @ [ quiescent_state ])
        | Fork (a1, a2) -> go (xs @ [ a2; a1 ]))
  in
  go

(* let () = sched [ prog ] *)

(* handling the sequencing effect *)
let write_computation (s : string) (k : action) : action =
  let rec go input =
    match input with
    | [] -> k
    | x :: xs ->
        Atom
          (fun () ->
            Printf.printf "%c%!" x;
            go xs)
  in
  let input = s |> String.to_seq |> List.of_seq in
  go input

let prog3 = write_computation "Hello" (write_computation "CIS 552\n" Stop)

let sequence_computation (first : action -> action) (second : action -> action)
    : action -> action =
 fun k -> k |> second |> first

let hello552_computation =
  sequence_computation
    (write_computation "Hello")
    (write_computation "CIS 552\n")

(*
let () = sched [ hello552_computation Stop ]
let () = sched [ Fork (hello552_computation Stop, hello552_computation Stop) ]
let rec bomb () = write_computation "bomb" (bomb ())
*)

(* version 3 passing information from the first action to the second *)
(* in the case of write, no information is passed *)

let read_computation (k : string -> action) : action =
  let line = Stdlib.read_line () in
  k line

type 'a computation = ('a -> action) -> action

let sequence_computation_2 (m : 'a computation) (f : 'a -> 'b computation) :
    'b computation =
 fun cont_b -> m (fun a -> f a @@ cont_b)

let trivial_computation (a : 'a) : 'a computation = fun cont -> cont a

let map_computation (f : 'a -> 'b) (m : 'a computation) : 'b computation =
 fun cont_b -> m (fun a -> cont_b @@ f a)

let ( let* ) = sequence_computation_2
let ( >>= ) = sequence_computation_2
let ( >> ) a b = a >>= fun _ -> b
let return = trivial_computation
let ( let+ ) = map_computation

let atom (action : unit -> 'a) : 'a computation =
 fun cont ->
  Atom
    (fun () ->
      let result = action () in
      cont result)

let fork (process : unit computation) : unit computation =
 fun cont -> Fork (process (fun () -> Stop), cont ())

let run (prog : 'a computation) : unit = sched [ prog (fun _ -> Stop) ]
let write input = atom (fun () -> Stdlib.print_string input)

let rec infloop input : unit computation =
  let* _ = write input in
  infloop input

let example =
  let* _ = write "it's raining" in
  let* _ = fork (infloop "dog\n") in
  infloop "cat\n"

let ready_read () =
  let read_ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
  (*   match read_ready with [] -> None | _ -> try Some (read_line ()) with | End_of_file -> None *)
  match read_ready with
  | [] -> None
  | _ -> ( try Some (read_line ()) with End_of_file -> None)

let rec ioloop s =
  let line = ready_read () in
  match line with
  | None ->
      print_string s;
      ioloop s
  | Some line -> print_string line

let input : string option computation = atom ready_read

let rec ioloop s =
  let* line = input in
  match line with
  | None ->
      let* _ = write s in
      ioloop s
  | Some x -> return @@ "Thread " ^ s ^ ":" ^ x

let example_2 =
  let* () = fork (ioloop "a" >>= write) in
  ioloop "b" >>= write

let newMVar () : 'a option ref computation = atom (fun () -> ref None)

let writeMVar (reference : 'a option ref) (value : 'a) : unit computation =
  atom (fun () -> reference := Some value)

let takeMVar (reference : 'a option ref) : 'a option computation =
  atom (fun () ->
      let x = !reference in
      reference := None;
      x)

(* let () = run example_2 *)
type msg = Add | Reset | Print | Quit

let rec simulation reference i =
  let* msg = takeMVar reference in
  match msg with
  | Some Add ->
      let* () = write "Adding..\n" in
      simulation reference (i + 1)
  | Some Reset ->
      let* () = write "Resetting..\n" in
      simulation reference 0
  | Some Print ->
      let* () = write @@ "Current value is " ^ string_of_int i ^ "\n" in
      simulation reference i
  | Some Quit -> write "Done"
  | None -> simulation reference i

let interface reference get_input : unit computation =
  let rec loop reference =
    let* control = get_input in
    match control with
    | Some "a" -> writeMVar reference Add >> loop reference
    | Some "r" -> writeMVar reference Reset >> loop reference
    | Some "p" -> writeMVar reference Print >> loop reference
    | Some "q" -> writeMVar reference Quit >> loop reference
    | Some s -> write @@ "Unknown Command:" ^ s >> loop reference
    | None -> loop reference
  in
  loop reference

let prog =
  let* reference = newMVar () in
  let* () = fork (simulation reference 0) in
  interface reference input

let () = run prog

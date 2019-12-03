open Core

type op =
    | Add of int * int * int (* a, b, dest *)
    | Mul of int * int * int
    | Halt

let make_tape (prog: string): int array =
    String.split ~on:',' prog
    |> List.map ~f:(fun snum -> String.strip snum |> Int.of_string)
    |> Array.of_list

exception IllegalOp of int

let rec parse (tape: int array) (ops: op list) (i: int): op list =
    if i >= Array.length tape then ops else
    match tape.(i) with
    | 1 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        parse tape (Add (a, b, dest) :: ops) (i + 4)
    )
    | 2 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        parse tape (Mul (a, b, dest) :: ops) (i + 4)
    )
    | 99 -> parse tape (Halt :: ops) (i + 1)
    | op -> raise (IllegalOp op)
let parse (tape: int array): op list = List.rev (parse tape [] 0)

let rec eval (tape: int array) (ops: op list): unit =
    match ops with
    | Add (a, b, dest) :: t -> (
        tape.(dest) <- tape.(a) + tape.(b);
        eval tape t
    )
    | Mul (a, b, dest) :: t -> (
        tape.(dest) <- tape.(a) * tape.(b);
        eval tape t
    )
    | Halt :: _ -> ()
    | [] -> ()

let eval_mem mem =
    let ops = parse mem in
    eval mem ops;
    mem.(0)

let try_combo noun verb mem =
    let new_mem = Array.copy mem in
    new_mem.(1) <- noun;
    new_mem.(2) <- verb;
    eval_mem new_mem


let solve () =
    let prog = In_channel.read_all "inputs/2.txt" in
    let tape = make_tape prog in
    let noun = ref (-1) in
    let verb = ref (-1) in
    let ans = ref 0 in
    while !ans <> 19690720 && !noun < 99; do
        incr noun;
        verb := -1;
        while !ans <> 19690720 && !verb < 99; do
            incr verb;
            ans := try_combo !noun !verb tape;
        done;
        ans := try_combo !noun !verb tape;
    done;
    printf "%d\n" (100 * !noun + !verb)
open Core

type mode = Addr | Imm

exception IllegalMode
let to_mode n =
    match n with
    | 0 -> Addr
    | 1 -> Imm
    | _ -> raise IllegalMode

type op =
    | Add of (int * mode) * (int * mode) * (int * mode)
    | Mul of (int * mode) * (int * mode) * (int * mode)
    | Input of int
    | Output of (int * mode)
    | JumpIfTrue of (int * mode) * (int * mode)
    | JumpIfFalse of (int * mode) * (int * mode)
    | Lt of (int * mode) * (int * mode) * (int * mode)
    | Eq of (int * mode) * (int * mode) * (int * mode)
    | Halt

let read (arg, mode) tape =
    match mode with
    | Addr -> tape.(arg)
    | Imm -> arg

let write ~value:value ~addr:addr tape = tape.(addr) <- value

let make_tape (prog: string): int array =
    String.split ~on:',' prog
    |> List.map ~f:(fun snum -> String.strip snum |> Int.of_string)
    |> Array.of_list

exception IllegalOpCodeFormat of int
let normalize opcode =
    match opcode with
    | op' when 0 <= op' && op' <= 99 -> 0, 0, 0, op'
    | op' when 99 < op' && op' <= 999 -> 0, 0, (op' / 100), (op' mod 100)
    | op' when 999 < op' && op' <= 9999 -> 0, (op' / 1000), (op' / 100 mod 10), (op' mod 100)
    | op' when 9999 < op' && op' <= 99999 -> (op' / 10000), (op' / 1000 mod 100), (op' / 100 mod 10), (op' mod 100)
    | _ -> raise (IllegalOpCodeFormat opcode)

exception IllegalOpCode of int * int * int * int
let parse (tape: int array) (i: int): op * int =
    let opcode = normalize tape.(i) in
    match opcode with
    | dm, bm, am, 1 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        (Add ((a, (to_mode am)), (b, (to_mode bm)), (dest, (to_mode dm)))), (i + 4)
    )
    | dm, bm, am, 2 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        (Mul ((a, (to_mode am)), (b, (to_mode bm)), (dest, (to_mode dm)))), (i + 4)
    )
    | _, _, _, 3 -> (
        let dest = tape.(i + 1) in
        (Input dest), (i + 2)
    )
    | _, _, am, 4 -> (
        let a = tape.(i + 1) in
        (Output (a, (to_mode am))), (i + 2)
    )
    | _, bm, am, 5 -> (
        let test = tape.(i + 1) in
        let dest = tape.(i + 2) in
        (JumpIfTrue ((test, (to_mode am)), (dest, (to_mode bm)))), (i + 3)
    )
    | _, bm, am, 6 -> (
        let test = tape.(i + 1) in
        let dest = tape.(i + 2) in
        (JumpIfFalse ((test, (to_mode am)), (dest, (to_mode bm)))), (i + 3)
    )
    | dm, bm, am, 7 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        (Lt ((a, (to_mode am)), (b, (to_mode bm)), (dest, (to_mode dm)))), (i + 4)
    )
    | dm, bm, am, 8 -> (
        let a = tape.(i + 1) in
        let b = tape.(i + 2) in
        let dest = tape.(i + 3) in
        (Eq ((a, (to_mode am)), (b, (to_mode bm)), (dest, (to_mode dm)))), (i + 4)
    )
    | _, _, _, 99 -> Halt, (i + 1)
    | a, b, c, d -> raise (IllegalOpCode (a, b, c, d))

let rec eval (tape: int array) (ic: int): unit =
    let op, ic' = parse tape ic in
    match op with
    | Add (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(a + b) ~addr:dest tape;
        eval tape ic'
    )
    | Mul (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(a * b) ~addr:dest tape;
        eval tape ic'
    )
    | Lt (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(if a < b then 1 else 0) ~addr:dest tape;
        eval tape ic'
    )
    | Eq (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(if a = b then 1 else 0) ~addr:dest tape;
        eval tape ic'
    )
    | Input dest -> (
        printf "Input:";
        let input = Out_channel.(flush stdout); Int.of_string In_channel.(input_line_exn stdin) in
        tape.(dest) <- input;
        eval tape ic'
    )
    | Output a -> (
        let a = read a tape in
        printf "%d\n" a;
        eval tape ic'
    )
    | JumpIfTrue (test, dest) -> (
        let test = read test tape in
        let dest = read dest tape in
        match test with
        | 0 -> eval tape ic'
        | _ -> eval tape dest
    )
    | JumpIfFalse (test, dest) -> (
        let test = read test tape in
        let dest = read dest tape in
        match test with
        | 0 -> eval tape dest
        | _ -> eval tape ic'
    )
    | Halt -> ()

let solve () =
    let prog = In_channel.read_all "inputs/5.txt" in
    let tape = make_tape prog in
    eval tape 0
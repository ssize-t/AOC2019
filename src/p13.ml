open Core

let input = ref 0
let output = ref 0

type mode = Addr | Imm | Rel
[@@deriving show]

exception IllegalMode of int
let to_mode n =
    match n with
    | 0 -> Addr
    | 1 -> Imm
    | 2 -> Rel
    | n -> raise (IllegalMode n)

type op =
    | Add of (int * mode) * (int * mode) * (int * mode)
    | Mul of (int * mode) * (int * mode) * (int * mode)
    | Input of (int * mode)
    | Output of (int * mode)
    | JumpIfTrue of (int * mode) * (int * mode)
    | JumpIfFalse of (int * mode) * (int * mode)
    | Lt of (int * mode) * (int * mode) * (int * mode)
    | Eq of (int * mode) * (int * mode) * (int * mode)
    | Rbo of (int * mode)
    | Halt
[@@deriving show]

let read (arg, mode) tape rb =
    match mode with
    | Addr -> tape.(arg)
    | Imm -> arg
    | Rel -> tape.(arg + rb)

let write value (addr, mode) tape rb =
    match mode with
    | Addr -> tape.(addr) <- value
    | Imm -> failwith "Cannot write in immediate mode"
    | Rel -> tape.(addr + rb) <- value

let make_tape (prog: string): int array =
    let program = String.split ~on:',' prog
    |> List.map ~f:(fun snum -> String.strip snum |> Int.of_string)
    |> Array.of_list in
    Array.concat [program; Array.create ~len:10000 0]

exception IllegalOpCodeFormat of int
let normalize opcode =
    match opcode with
    | op' when 0 <= op' && op' <= 99 -> 0, 0, 0, op'
    | op' when 99 < op' && op' <= 999 -> 0, 0, (op' / 100), (op' mod 100)
    | op' when 999 < op' && op' <= 9999 -> 0, (op' / 1000), (op' / 100 mod 10), (op' mod 100)
    | op' when 9999 < op' && op' <= 99999 -> (op' / 10000), (op' / 1000 mod 100 mod 10), (op' / 100 mod 10), (op' mod 100)
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
    | _, _, am, 3 -> (
        let dest = tape.(i + 1) in
        (Input (dest, (to_mode am))), (i + 2)
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
    | _, _, am, 9 -> (
        let a = tape.(i + 1) in
        (Rbo (a, (to_mode am))), (i + 2)
    )
    | _, _, _, 99 -> Halt, (i + 1)
    | a, b, c, d -> raise (IllegalOpCode (a, b, c, d))

let rec eval (tape: int array) (ic: int) (rb: int) (halt_on_input: bool) (halt_on_output: bool): int * int * bool =
    let op, ic' = parse tape ic in
    match op with
    | Add (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a + b)  (dest, dm) tape rb;
        eval tape ic' rb halt_on_input halt_on_output
    )
    | Mul (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a * b)  (dest, dm) tape rb;
        eval tape ic' rb halt_on_input halt_on_output
    )
    | Lt (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a < b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb halt_on_input halt_on_output
    )
    | Eq (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a = b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb halt_on_input halt_on_output
    )
    | Input dest -> (
        match halt_on_input with
        | true -> ic, rb, false
        | false -> (
            write !input dest tape rb;
            eval tape ic' rb halt_on_input halt_on_output
        )
    )
    | Output a -> (
        let a = read a tape rb in
        output := a;
        match halt_on_output with
        | true -> ic', rb, false
        | false -> eval tape ic' rb halt_on_input halt_on_output
    )
    | JumpIfTrue (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape ic' rb halt_on_input halt_on_output
        | _ -> eval tape dest rb halt_on_input halt_on_output
    )
    | JumpIfFalse (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape dest rb halt_on_input halt_on_output
        | _ -> eval tape ic' rb halt_on_input halt_on_output
    )
    | Rbo offset -> (
        let offset = read offset tape rb in
        eval tape ic' (rb + offset) halt_on_input halt_on_output
    )
    | Halt -> ic, rb, true

type tile =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
let tile_of_int i =
    match i with
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | _ -> failwith "Invalid tile"
let tile_to_int t =
    match t with
    | Empty -> 0
    | Wall -> 1
    | Block -> 2
    | Paddle -> 3
    | Ball -> 4

type pos = int * int
let x p = fst p
let y p = snd p
let pos_to_string ((x,y): pos) = sprintf "%d,%d" x y

let set tbl k v =
    (match Hashtbl.find tbl k with
    | Some _ -> 
        let _ = Hashtbl.remove tbl k in ()
    | None -> ());
    let _ = Hashtbl.add tbl k v in ()

let run (tape: int array) =
    let ic = ref 0 in
    let rb = ref 0 in
    let halted = ref false in
    let pixels = Hashtbl.create (module String) in
    while not !halted do
        let (ic', rb', hlt) = eval tape !ic !rb false true in
        let x = !output in
        let (ic', rb', hlt) = eval tape ic' rb' false true in
        let y = !output in
        let (ic', rb', hlt) = eval tape ic' rb' false true in
        let tid = !output in
        ic := ic';
        rb := rb';
        halted := hlt;
        set pixels (pos_to_string (x,y)) (tile_of_int tid)
    done;
    Hashtbl.to_alist pixels |> List.filter ~f:(fun (_, t) -> (
        match t with
        | Block -> true
        | _ -> false
    )) |> List.length |> printf "%d\n"

let solve () =
    let prog = In_channel.read_all "inputs/13.txt" in
    let tape = make_tape prog in
    run tape
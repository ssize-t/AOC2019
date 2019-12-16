open Core

let xaddr = 388
let xaccaddr = 390
let yaddr = 389
let yaccaddr = 391

let bottom_boundary = 20

let width = 44
let height = 24

type pos = int * int
let x p = fst p
let y p = snd p
let pos_to_string ((x,y): pos) = sprintf "%d,%d" x y
let pos_of_string s =
    match String.split ~on:',' s with
    | [x;y] -> (Int.of_string x), (Int.of_string y)
    | _ -> failwith "Malformed pos"

type jpos = Neutral | Left | Right
let jpos_to_int j =
    match j with
    | Neutral -> 0
    | Left -> -1
    | Right -> 1
let jpos_of_char c =
    match c with
    | Some 'a' -> Left
    | Some 'd' -> Right
    | _ -> Neutral

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
let tile_to_string t =
    match t with
    | Empty -> " "
    | Wall -> "|"
    | Block -> "#"
    | Paddle -> "="
    | Ball -> "o"

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

let last_read_val = ref 0
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


let get1char () =
    let open Unix.Terminal_io in
    let termio = tcgetattr Unix.stdin in
    let () =
        tcsetattr { termio with c_icanon = false } Unix.stdin TCSADRAIN
    in
    let res = In_channel.input_char In_channel.stdin in
    tcsetattr termio Unix.stdin TCSADRAIN;
    res

let paused = ref false
let input_loop () =
    while true do
        let input = get1char () in
        match input with
        | Some 'c' -> paused := true
        | Some 'v' -> paused := false
        | _ -> ()
    done

let image =
    let im = Array.create ~len:height [||] in
    let y = ref 0 in
    while !y <> height do
        im.(!y) <- (Array.create ~len:width "+");
        incr y
    done;
    im

type game_state = X | Y | Tile
let x = ref 0
let y = ref 0
let score = ref 0

let draw () =
    printf "Score: %d\n%s\n" (!score) ((image |> Array.map ~f:(fun line -> line |> Array.to_list |> String.concat)) |> Array.to_list |> String.concat ~sep:"\n");
    Out_channel.flush stdout

let is = ref 0
let rec eval (tape: int array) (ic: int) (rb: int) (s: game_state): int * int * bool =
    (* draw (); *)
    if (tape.(yaddr) >= bottom_boundary) then (
        (* Hehe :-) *)
        tape.(yaccaddr) <- if tape.(yaccaddr) = 1 then -1 else 1
    );
    let op, ic' = parse tape ic in
    match op with
    | Add (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a + b)  (dest, dm) tape rb;
        eval tape ic' rb s
    )
    | Mul (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a * b)  (dest, dm) tape rb;
        eval tape ic' rb s
    )
    | Lt (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a < b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb s
    )
    | Eq (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a = b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb s
    )
    | Input dest -> (
        incr is;
        if !paused then (
            input := get1char () |> jpos_of_char |> jpos_to_int;
        ) else input := Neutral |> jpos_to_int;

        write !input dest tape rb;
        eval tape ic' rb s
    )
    | Output a -> (
        let a = read a tape rb in
        let s' = match s with
            | X -> (x := a; Y)
            | Y -> (y := a; Tile)
            | Tile -> (
                match !x, !y with
                | -1, 0 -> score := a; X
                | x, y -> image.(y).(x) <- (a |> tile_of_int |> tile_to_string); X
            ) in
        output := a;
        eval tape ic' rb s'
    )
    | JumpIfTrue (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape ic' rb s
        | _ -> eval tape dest rb s
    )
    | JumpIfFalse (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape dest rb s
        | _ -> eval tape ic' rb s
    )
    | Rbo offset -> (
        let offset = read offset tape rb in
        eval tape ic' (rb + offset) s
    )
    | Halt -> ic, rb, true

let solve () =
    let prog = In_channel.read_all "inputs/13.txt" in
    let tape = make_tape prog in
    (* let il = Thread.create input_loop () in *)
    let _, _, _ = eval tape 0 0 X in printf "%d\n" !score
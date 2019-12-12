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

type color = White | Black
let color_of_int i =
    match i with
    | 0 -> Black
    | 1 -> White
    | _ -> failwith "Unknown color"
let color_to_int c =
    match c with
    | Black -> 0
    | White -> 1
let color_to_string c =
    match c with
    | Black -> "Black"
    | White -> "White"

type turn_dir = Left | Right
let dir_of_int i =
    match i with
    | 0 -> Left
    | 1 -> Right
    | _ -> failwith "Unknown direction"
let dir_to_int d =
    match d with
    | Left -> 0
    | Right -> 1
type dir = U | D | L | R
let dir_to_string d =
    match d with | U -> "U" | D -> "D" | L -> "L" | R -> "R"

type pos = int * int
let pos_to_string p = sprintf "%d,%d" (fst p) (snd p)
let pos_of_string s =
    match String.split s ~on:',' with
    | [x;y] -> (Int.of_string x),(Int.of_string y)
    | _ -> failwith "Malformed pos"
let (++) ((x,y): pos) ((x',y'): pos) = (x + x', y + y')
let move (p: pos) (d: dir) (turn: turn_dir) =
    let off, new_dir = match d, turn with
        | U, Left -> (-1, 0), L
        | U, Right -> (1, 0), R
        | D, Left -> (1, 0), R
        | D, Right -> (-1, 0), L
        | L, Left -> (0, -1), D
        | L, Right -> (0, 1), U
        | R, Left -> (0, 1), U
        | R, Right -> (0, -1), D
    in
    p ++ off, new_dir

let unknown_color = ref 0
let get_color (pixels: (string, color) Hashtbl.t) (p: pos) =
    match Hashtbl.find pixels (pos_to_string p) with
    | Some c -> (color_to_int c)
    | None -> incr unknown_color; color_to_int Black

let already_set = ref 0
let set_color (pixels: (string, color) Hashtbl.t) (p: pos) (c: color) =
    let p = pos_to_string p in
    match Hashtbl.find pixels p with
    | Some c' ->
        incr already_set;
        Hashtbl.remove pixels p;
        Hashtbl.set pixels p c
    | None -> Hashtbl.set pixels p c

let show_image (pixels: (string, color) Hashtbl.t) =
    let pixels_vals = Hashtbl.keys pixels |> List.map ~f:pos_of_string in
    let unwrap a = match a with | Some a -> a | None -> failwith "unwrap" in
    let minY = List.min_elt pixels_vals ~compare:(fun (_, y) (_, y') -> y - y') |> unwrap |> snd in
    let maxY = List.max_elt pixels_vals ~compare:(fun (_, y) (_, y') -> y - y') |> unwrap |> snd in
    let minX = List.min_elt pixels_vals ~compare:(fun (x, _) (x', _) -> x - x') |> unwrap |> fst in
    let maxX = List.max_elt pixels_vals ~compare:(fun (x, _) (x', _) -> x - x') |> unwrap |> fst in
    let p = ref (minX, maxY) in
    let row = ref [] in
    while !p <> (minX, minY - 1) do
        let c = get_color pixels !p |> color_of_int in
        (match c with
        | Black -> row := !row @ ["##"]; printf "##"
        | White -> row := !row @ ["  "]; printf "  ");
        match !p with
        | (x, y) when x = maxX && y = minY - 1 -> ()
        | (x, y) when x = maxX -> printf "\n%s\n" (!row |> String.concat); row := []; p := (minX, y - 1)
        | (x, y) -> p := (x + 1, y)
    done

let solve () =
    let prog = In_channel.read_all "inputs/11.txt" in
    let tape = make_tape prog in
    let pixels: (string, color) Hashtbl.t = Hashtbl.create (module String) in
    set_color pixels (0, 0) White;
    let p = ref (0,0) in
    let d = ref U in
    let ic = ref 0 in
    let rb = ref 0 in
    let halted = ref false in
    let count = ref 0 in
    while not !halted do
        incr count;
        input := get_color pixels !p;
        let (ic', rb', hlt) = eval tape !ic !rb false true in
        let color = color_of_int !output in
        let (ic', rb', hlt) = eval tape ic' rb' false true in
        let dir = dir_of_int !output in
        set_color pixels !p color;
        let p', d' = move !p !d dir in
        p := p';
        d := d';
        ic := ic';
        rb := rb';
        halted := hlt
    done;
    show_image pixels
    (* printf "%d\n" (Hashtbl.length pixels) *)
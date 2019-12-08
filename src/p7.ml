open Core

let inputs = ref []
let output_val = ref 0

let input () =
    match !inputs with
    | [] ->
        printf "Input: ";
        Out_channel.(flush stdout);
        Int.of_string In_channel.(input_line_exn stdin)
    | h :: t -> inputs := t; h

let output v = output_val := v

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

let rec eval (tape: int array) (ic: int) halt_on_output halt_on_input: int =
    let op, ic' = parse tape ic in
    match op with
    | Add (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(a + b) ~addr:dest tape;
        eval tape ic' halt_on_output halt_on_input
    )
    | Mul (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(a * b) ~addr:dest tape;
        eval tape ic' halt_on_output halt_on_input
    )
    | Lt (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(if a < b then 1 else 0) ~addr:dest tape;
        eval tape ic' halt_on_output halt_on_input
    )
    | Eq (a, b, (dest, _)) -> (
        let a = read a tape in
        let b = read b tape in
        write ~value:(if a = b then 1 else 0) ~addr:dest tape;
        eval tape ic' halt_on_output halt_on_input
    )
    | Input dest -> (
        let input = input () in
        tape.(dest) <- input;
        match halt_on_input with
        | false -> eval tape ic' halt_on_output halt_on_input
        | true -> ic'
    )
    | Output a -> (
        let a = read a tape in
        output a;
        match halt_on_output with
        | false -> eval tape ic' halt_on_output halt_on_input
        | true -> ic'
    )
    | JumpIfTrue (test, dest) -> (
        let test = read test tape in
        let dest = read dest tape in
        match test with
        | 0 -> eval tape ic' halt_on_output halt_on_input
        | _ -> eval tape dest halt_on_output halt_on_input
    )
    | JumpIfFalse (test, dest) -> (
        let test = read test tape in
        let dest = read dest tape in
        match test with
        | 0 -> eval tape dest halt_on_output halt_on_input
        | _ -> eval tape ic' halt_on_output halt_on_input
    )
    | Halt -> -1

let try_phases tape p0 p1 p2 p3 p4: int =
    output_val := 0;
    let t0, t1, t2, t3, t4 = (Array.copy tape), (Array.copy tape), (Array.copy tape), (Array.copy tape), (Array.copy tape) in
    let s = ref 0 in
    let i0, i1, i2, i3, i4 = ref 0, ref 0, ref 0, ref 0, ref 0 in
    (* init *)
    inputs := [p0];
    i0 := eval t0 !i0 true true;
    inputs := [p1];
    i1 := eval t1 !i1 true true;
    inputs := [p2];
    i2 := eval t2 !i2 true true;
    inputs := [p3];
    i3 := eval t3 !i3 true true;
    inputs := [p4];
    i4 := eval t4 !i4 true true;
    while (!i0 <> -1 && !i1 <> -1 && !i2 <> -1 && !i3 <> -1 && !i4 <> -1) do
        inputs := [!s];
        i0 := eval t0 !i0 true false;
        let s0 = !output_val in
        inputs := [s0];
        i1 := eval t1 !i1 true false;
        let s1 = !output_val in
        inputs := [s1];
        i2 := eval t2 !i2 true false;
        let s2 = !output_val in
        inputs := [s2];
        i3 := eval t3 !i3 true false;
        let s3 = !output_val in
        inputs := [s3];
        i4 := eval t4 !i4 true false;
        s := !output_val
    done;
    !s

let rec gather_results
    tape
    (acc: (int * int) list)
    (thruster: int)
    (phases: int list)
    (res: (int * (int * int * int * int * int)) list)=
    match thruster with
    | 5 -> (
        match acc with
        | (4, 9) :: (3, 9) :: (2, 9) :: (1, 9) :: (0, 9) :: t -> res
        | (4, 9) :: (3, 9) :: (2, 9) :: (1, 9) :: (0, p0) :: t -> (
            let s = try_phases tape p0 9 9 9 9 in
            gather_results
                tape
                t
                0
                ((p0 + 1) :: 0 :: 0 :: 0 :: 0 :: phases)
                ((s, (p0, 9, 9, 9, 9)) :: res)
        )
        | (4, 9) :: (3, 9) :: (2, 9) :: (1, p1) :: (0, p0) :: t -> (
            let s = try_phases tape p0 p1 9 9 9 in
            gather_results
                tape
                ((0, p0) :: t)
                1
                ((p1 + 1) :: 0 :: 0 :: 0 :: phases)
                ((s, (p0, p1, 9, 9, 9)) :: res)
        )
        | (4, 9) :: (3, 9) :: (2, p2) :: (1, p1) :: (0, p0) :: t -> (
            let s = try_phases tape p0 p1 p2 9 9 in
            gather_results
                tape
                ((1, p1) :: (0, p0) :: t)
                2
                ((p2 + 1) :: 0 :: 0 :: phases)
                ((s, (p0, p1, p2, 9, 9)) :: res)
        )
        | (4, 9) :: (3, p3) :: (2, p2) :: (1, p1) :: (0, p0) :: t -> (
            let s = try_phases tape p0 p1 p2 p3 9 in
            gather_results
                tape
                ((2, p2) :: (1, p1) :: (0, p0) :: t)
                3
                ((p3 + 1) :: 0 :: phases)
                ((s, (p0, p1, p2, p3, 9)) :: res)
        )
        | (4, p4) :: (3, p3) :: (2, p2) :: (1, p1) :: (0, p0) :: t -> (
            let s = try_phases tape p0 p1 p2 p3 p4 in
            gather_results
                tape
                ((3, p3) :: (2, p2) :: (1, p1) :: (0, p0) :: t)
                4
                ((p4 + 1) :: phases)
                ((s, (p0, p1, p2, p3, p4)) :: res)
        )
        | a -> printf "a: "; List.iter a ~f:(fun (a, b) -> printf "(%d, %d) " a b);
            failwith "Invalid accumulator in gather_results"
    )
    | _ -> (
        let phase = List.hd_exn phases in
        gather_results tape((thruster, phase) :: acc) (thruster + 1) (List.tl_exn phases) res
    )

let gather_results tape =
    gather_results tape [] 0 [5;5;5;5;5] []


let solve () =
    let prog = In_channel.read_all "inputs/7.txt" in
    let tape = make_tape prog in
    match gather_results tape
    |> List.filter ~f:(fun (_, a) -> match a with | a, b, c, d, e when Set.of_list (module Int) [a;b;c;d;e] |> Set.length = 5 -> true | _ -> false)
    |> List.max_elt ~compare:(fun (s, _) (s', _) -> s - s') with
    | Some (s, (p0, p1, p2, p3, p4)) -> (
        printf "[%d]: %d, %d, %d, %d, %d\n" s p0 p1 p2 p3 p4
    )
    | None -> printf "No max found"
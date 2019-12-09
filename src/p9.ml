open Core

let debug_program = false

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

let rec eval (tape: int array) (ic: int) (rb: int): unit =
    let op, ic' = parse tape ic in
    match op with
    | Add (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a + b)  (dest, dm) tape rb;
        eval tape ic' rb
    )
    | Mul (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (a * b)  (dest, dm) tape rb;
        eval tape ic' rb
    )
    | Lt (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a < b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb
    )
    | Eq (a, b, (dest, dm)) -> (
        let a = read a tape rb in
        let b = read b tape rb in
        write (if a = b then 1 else 0)  (dest, dm) tape rb;
        eval tape ic' rb
    )
    | Input dest -> (
        printf "Input:";
        let input = Out_channel.(flush stdout); Int.of_string In_channel.(input_line_exn stdin) in
        write input dest tape rb;
        eval tape ic' rb
    )
    | Output a -> (
        let a = read a tape rb in
        printf "%d\n" a;
        eval tape ic' rb
    )
    | JumpIfTrue (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape ic' rb
        | _ -> eval tape dest rb
    )
    | JumpIfFalse (test, dest) -> (
        let test = read test tape rb in
        let dest = read dest tape rb in
        match test with
        | 0 -> eval tape dest rb
        | _ -> eval tape ic' rb
    )
    | Rbo offset -> (
        let offset = read offset tape rb in
        eval tape ic' (rb + offset)
    )
    | Halt -> ()

type command =
    Step
    | PrintTape
    | PrintIc
    | PrintRb
    | PrintOpcode
    | PrintIcs
    | PrintRbs
    | Bt
    | Run
    | ParseAddr
    | PrintAddr
    | Parse
    | Eval
    | EvalAddr
let rbs = ref None
let ics = ref None
let store_rb rb =
    match !rbs with
    | None -> rbs := Some [rb]
    | Some rbs' -> rbs := Some (rb :: rbs') 
let store_ic ic =
    match !ics with
    | None -> ics := Some [ic]
    | Some ics' -> ics := Some (ic :: ics')
let stored_ics () =
    match !ics with
    | None -> []
    | Some ics' -> ics'
let stored_rbs () =
    match !rbs with
    | None -> []
    | Some rbs' -> rbs'
let pop_ic () =
    match !ics with
    | None -> None
    | Some [] -> None
    | Some (h :: t) -> ics := Some t; Some h
let pop_rb () =
    match !ics with
    | None -> None
    | Some [] -> None
    | Some (h :: t) -> ics := Some t; Some h

let rec debug (tape: int array) (ic: int) (rb: int) (prev_command: command option): unit =
    let eval op callback command ic' =
        match op with
        | Add (a, b, (dest, dm)) -> (
            let a = read a tape rb in
            let b = read b tape rb in
            write (a + b)  (dest, dm) tape rb;
            callback ic' rb !command
        )
        | Mul (a, b, (dest, dm)) -> (
            let a = read a tape rb in
            let b = read b tape rb in
            write (a * b)  (dest, dm) tape rb;
            callback ic' rb !command
        )
        | Lt (a, b, (dest, dm)) -> (
            let a = read a tape rb in
            let b = read b tape rb in
            write (if a < b then 1 else 0)  (dest, dm) tape rb;
            callback ic' rb !command
        )
        | Eq (a, b, (dest, dm)) -> (
            let a = read a tape rb in
            let b = read b tape rb in
            write (if a = b then 1 else 0)  (dest, dm) tape rb;
            callback ic' rb !command
        )
        | Input dest -> (
            printf "Input:";
            let input = Out_channel.(flush stdout); Int.of_string In_channel.(input_line_exn stdin) in
            write input dest tape rb;
            callback ic' rb !command
        )
        | Output a -> (
            let a = read a tape rb in
            printf "%d\n" a;
            callback ic' rb !command
        )
        | JumpIfTrue (test, dest) -> (
            let test = read test tape rb in
            let dest = read dest tape rb in
            match test with
            | 0 -> callback ic' rb !command
            | _ -> callback dest rb !command
        )
        | JumpIfFalse (test, dest) -> (
            let test = read test tape rb in
            let dest = read dest tape rb in
            match test with
            | 0 -> callback dest rb !command
            | _ -> callback ic' rb !command
        )
        | Rbo offset -> (
            let offset = read offset tape rb in
            callback ic' (rb + offset) !command
        )
        | Halt -> () in
    let prompt () =
        printf ">";
        match
            Out_channel.(flush stdout); In_channel.(input_line_exn stdin) with
            | "s" | "step" -> Some Step
            | "pt" | "print tape" -> Some PrintTape
            | "pic" | "print ic" -> Some PrintIc
            | "prb" | "print rb" -> Some PrintRb
            | "po" | "print opcode" -> Some PrintOpcode
            | "pics" | "print ics" -> Some PrintIcs
            | "prbs" | "print rbs" -> Some PrintRbs
            | "bt" | "backtrack" -> Some Bt
            | "r" | "run" -> Some Run
            | "paddr" | "parse addr" -> Some ParseAddr
            | "praddr" | "print addr" -> Some PrintAddr
            | "p" | "parse" -> Some Parse
            | "e" | "eval" -> Some Eval
            | "eaddr" | "eval addr" -> Some EvalAddr
            | _ -> printf "Unknown command\n"; None in
    let op, ic' = parse tape ic in
    let command_done = ref false in
    let command = ref prev_command in
    let _ = match prev_command with
    | Some Run -> ()
    | _ -> (
        while not !command_done do
            let cmd = prompt () in
            command := cmd;
            match cmd with
            | Some PrintIc -> printf "i: %d\n" ic
            | Some PrintRb -> printf "rb: %d\n" rb
            | Some PrintTape -> (
                let s = if ic > 5 then ic - 5 else ic in
                let e = ic + 5 in
                let i = ref s in
                while !i <> e do
                    printf "[%d] %d " !i tape.(!i);
                    incr i
                done;
                printf "\n"
            )
            | Some PrintOpcode -> printf "%s\n" (show_op op);
            | Some Step -> command_done := true;
            | Some PrintRbs -> (
                let rbs = stored_rbs () in
                let count = ref (if List.length rbs > 5 then 5 else List.length rbs) in
                while !count <> 0 do
                    let rb' = (List.to_array rbs).(!count) in
                    printf " %d " rb';
                    decr count
                done;
                printf "\n"
            )
            | Some PrintIcs -> (
                let ics = stored_ics () in
                let count = ref (if List.length ics > 5 then 5 else List.length ics) in
                while !count <> 0 do
                    let ic' = (List.to_array ics).(!count) in
                    printf " %d " ic';
                    decr count
                done;
                printf "\n"
            )
            | Some Bt -> (
                let ic' = pop_ic () in
                let rb' = pop_rb () in
                match ic', rb' with
                    | None, None -> printf "Cannot backtrack"
                    | _, None -> printf "Cannot backtrack"
                    | None, _ -> printf "Cannot backtrack"
                    | Some ic', Some rb' -> printf "New ic: %d, new rb: %d\n" ic' rb'; debug tape ic' rb' !command
            )
            | Some Run -> command_done := true
            | Some ParseAddr -> (
                printf "Addr: ";
                let addr = Out_channel.(flush stdout); In_channel.(input_line_exn stdin) |> Int.of_string in
                let op, ic' = parse tape addr in
                printf "[%d]: %s => [%d]\n" addr (show_op op) ic'
            )
            | Some PrintAddr -> (
                printf "Addr: ";
                let addr = Out_channel.(flush stdout); In_channel.(input_line_exn stdin) |> Int.of_string in
                printf "[%d]: %d\n" addr tape.(addr)
            )
            | Some Parse -> (
                printf "Input: ";
                let input = Out_channel.(flush stdout); In_channel.(input_line_exn stdin) |> Int.of_string in
                let op, ic' = parse [| input |] 0 in
                printf "%s => [+%d]\n" (show_op op) ic'
            )
            | Some Eval -> (
                printf "Input: ";
                let input = Out_channel.(flush stdout); In_channel.(input_line_exn stdin) |> Int.of_string in
                let op, ic'' = parse [| input |] 0 in
                eval op (fun ic' rb' command' -> printf "%s => [+%d] { ic': %d, rb': %d }\n" (show_op op) ic'' ic' rb') command ic'
            )
            | Some EvalAddr -> (
                printf "Addr: ";
                let addr = Out_channel.(flush stdout); In_channel.(input_line_exn stdin) |> Int.of_string in
                let op, ic'' = parse tape addr in
                eval op (fun ic' rb' command' -> printf "[%d] %s => [+%d] { ic': %d, rb': %d }\n" addr (show_op op) ic'' ic' rb') command ic'
            )
            | None -> ()
        done;
    ) in
    store_ic ic;
    store_rb rb;
    try
        eval op (fun ic' rb' command' -> debug tape ic' rb' command') command ic'
    with a -> printf "Exception: %s\n" (Exn.to_string a); debug tape ic' rb None



let solve () =
    let prog = In_channel.read_all "inputs/9.txt" in
    let tape = make_tape prog in
    match debug_program with
    | false -> eval tape 0 0
    | true -> debug tape 0 0 None
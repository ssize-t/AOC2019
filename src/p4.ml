open Core

let rec two_adj_digits pass =
    match pass with
    | a :: b :: c :: d :: t when b = c && b <> a && b <> d -> true
    |  _ :: t -> two_adj_digits t
    | _ -> false
let two_adj_digits pass = 
    let head_test cpass =
        match cpass with
        | a :: b :: c :: t when a = b && a <> c -> true
        | _ -> false
    in
    let cpass = String.to_list pass in
    if head_test cpass || head_test (List.rev cpass) then true
    else two_adj_digits cpass
let rec non_decreasing_digits pass =
    match pass with
    | a :: b :: t when Char.to_int a <= Char.to_int b ->
        non_decreasing_digits (b :: t)
    | [] -> true
    | _ :: [] -> true
    | _ -> false
let non_decreasing_digits pass = String.to_list pass |> non_decreasing_digits

let validate pass =
    let pass = Int.to_string pass in
    two_adj_digits pass &&
    non_decreasing_digits pass

let enumerate lower upper =
    let cur = ref (lower + 1) in
    let num = ref 0 in
    while !cur < upper do
        if validate !cur then
            incr num;
        incr cur
    done;
    !num

exception FormatException
let solve () =
    match In_channel.read_all "inputs/4.txt"
    |> String.strip
    |> String.split ~on:'-'
    |> List.map ~f:Int.of_string with
    | lower :: upper :: [] -> (
        printf "%d\n" (enumerate lower upper)
    )
    | _ -> raise FormatException
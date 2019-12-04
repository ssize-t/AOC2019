open Core

let rec two_adj_digits pass =
    match pass with
    | a :: b :: t when Char.to_int a = Char.to_int b -> true
    |  _ :: t -> two_adj_digits t
    | _ -> false
let two_adj_digits pass = String.to_list pass |> two_adj_digits
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
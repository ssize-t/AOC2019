open Core

type cart = int * int
let x (c: cart): int = (fst c)
let y (c: cart): int = (snd c)
let minY (a: cart) (b: cart): cart =
    if y a < y b then a else b
let maxY (a: cart) (b: cart): cart =
    if y a > y b then a else b
let minX (a: cart) (b: cart): cart =
    if x a < x b then a else b
let maxX (a: cart) (b: cart): cart =
    if x a > x b then a else b
let cart_to_string (c: cart) =
    sprintf "%d,%d" (x c) (y c)

type graph = (string, string list) Hashtbl.t
let add_line (a: cart) (b: cart) (g: graph) =
    let a = cart_to_string a in
    let b = cart_to_string b in
    let add a b =
        match Hashtbl.find g a with
        | None -> let _ = Hashtbl.add g a [b] in ()
        | Some ac when List.filter ac ~f:(fun a -> a = b) |> List.length > 0 -> ()
        | Some ac -> Hashtbl.remove g a; let _ = Hashtbl.add g a (b :: ac) in () in
    add a b

let between (a: cart) (b: cart) (c: cart): bool =
    let minY = minY a b |> y in
    let minX = minX a b |> x in
    let maxY = maxY a b |> y in
    let maxX = maxX a b |> x in
    (x c) >= minX && (x c) <= maxX && (y c) >= minY && (y c) <= maxY

let on_same_line (a: cart) (b: cart) (c: cart): bool =
    let cross = ((y c) - (y a)) * ((x b) - (x a)) - ((x c) - (x a)) * ((y b) - (y a)) in
    match cross <> 0 with
    | true -> false
    | false -> (
        let dot = ((x c) - (x a)) * ((x b) - (x a)) + ((y c) - (y a)) * ((y b) - (y a)) in
        match dot < 0 with
        | true -> false
        | false -> (
            let sqlenba = ((x b) - (x a)) * ((x b) - (x a)) + ((y b) - (y a)) * ((y b) - (y a)) in
            dot <= sqlenba
        )
    )

let parse (lines: string list): cart list =
    lines
    |> List.mapi ~f:(fun y l -> (
        String.to_list l |> List.mapi ~f:(fun x c -> (
            match c with
            | '.' -> None
            | '#' -> Some (x, y)
            | _ -> failwith "Malformed data"
        ))
    ))
    |> List.concat
    |> List.filter ~f:(fun mp -> match mp with | None -> false | _ -> true)
    |> List.map ~f:(fun mp -> match mp with | Some p -> p | _ -> failwith "")

let show_distances (data: cart list) (g: graph) =
    let show_cart c =
        match Hashtbl.find g (cart_to_string c) with
        | Some d -> printf "%d" (List.length d)
        | None -> printf "." in
    let width =
        match List.max_elt data ~compare:(fun (x, _) (x', _) -> x - x') with
        | Some m -> (x m)
        | None -> failwith "" in
    let height =
        match List.max_elt data ~compare:(fun (_, y) (_, y') -> y - y') with
        | Some m -> (y m)
        | None -> failwith "" in
    let x = ref 0 in
    let y = ref 0 in
    while not (!x = width && !y = height + 1) do
        let c = (!x, !y) in
        show_cart c;
        if !x = width && not (!y = height + 1) then (
            printf "\n";
            if !y <> height then x := 0;
            incr y
        ) else incr x
    done

let solve () =
    let g: graph = Hashtbl.create (module String) in
    let points = In_channel.read_lines "inputs/10.txt"
    |> parse in
    List.cartesian_product points points
    |> List.iter ~f:(fun (a, b) -> (
        if a <> b then (
            if List.filter points ~f:(fun c -> c <> a && c <> b && between a b c && on_same_line a b c) |> List.length = 0 then (
                add_line a b g
            )
        )
    ));
    (* show_distances points g; *)
    match Hashtbl.to_alist g |> List.max_elt ~compare:(fun (a, ac) (b, bc) -> (List.length ac) - (List.length bc)) with
    | Some (a, ac) -> printf "%s [%d]\n" a (List.length ac)
    | None -> failwith "No max found"
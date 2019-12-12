open Core

type part = One | Two
let part = Two

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
let cart_of_string s =
    match String.split ~on:',' s with
    | [x; y] -> (Int.of_string x),(Int.of_string y)
    | _ -> failwith "Malformed serialized cart"

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

let show_distances (data: cart list) (g: (string, int) Hashtbl.t) =
    let show_cart c =
        match Hashtbl.find g (cart_to_string c) with
        | Some d -> if d < 10 then printf " %d " d else if d < 100 then printf " %d" d else printf "%d" d
        | None -> printf " . " in
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

type ring = cart list

let rec find_rings (g: graph) (queue: string list) (ring_acc: string list) (acc: ring list) (visited: (string, bool) Hashtbl.t) (visited_ring_acc: (string, bool) Hashtbl.t): ring list =
    (* printf "ring_acc: "; List.iter ring_acc ~f:(fun s -> printf "(%s) " s); printf "\n";
    printf "acc: "; List.iter acc ~f:(fun acc -> printf "["; List.iter acc ~f:(fun s -> printf "(%s) " (cart_to_string s)); printf "] "); printf "\n"; *)
    match queue with
    | [] -> acc
    | [ current ] -> (
        match Hashtbl.find visited current with
        | Some _ -> (
            find_rings
                g
                ring_acc
                []
                ((List.map ring_acc ~f:(fun c -> cart_of_string c)) :: acc)
                visited
                visited_ring_acc
        )
        | None -> (
            match Hashtbl.find g current with
            | Some ring -> (
                let ring = List.filter ring ~f:(fun c -> (
                    match Hashtbl.find visited c with
                    | Some _ -> false
                    | None -> match Hashtbl.find visited_ring_acc c with | Some _ -> false | None -> true
                )) in
                let _ = Hashtbl.add visited current true in
                List.iter ring ~f:(fun r -> let _ = Hashtbl.add visited_ring_acc r true in ());
                let new_ring = ring @ ring_acc in
                find_rings
                    g
                    new_ring
                    []
                    ((List.map new_ring ~f:(fun c -> cart_of_string c)) :: acc)
                    visited
                    visited_ring_acc
            )
            | None -> (
                let _ = Hashtbl.add visited current true in
                find_rings
                    g
                    ring_acc
                    []
                    ((List.map ring_acc ~f:(fun c -> cart_of_string c)) :: acc)
                    visited
                    visited_ring_acc
            )
        )
    )
    | current :: t -> (
        match Hashtbl.find visited current with
        | Some _ -> find_rings g t [] acc visited visited_ring_acc
        | None -> (
            match Hashtbl.find g current with
            | Some ring -> (
                let ring = List.filter ring ~f:(fun c -> (
                    match Hashtbl.find visited c with
                    | Some _ -> false
                    | None -> match Hashtbl.find visited_ring_acc c with | Some _ -> false | None -> true
                )) in
                let _ = Hashtbl.add visited current true in
                List.iter ring ~f:(fun r -> let _ = Hashtbl.add visited_ring_acc r true in ());
                let new_ring = ring @ ring_acc in
                find_rings
                    g
                    t
                    new_ring
                    acc
                    visited
                    visited_ring_acc
            )
            | None -> (
                let _ = Hashtbl.add visited current true in
                find_rings
                    g
                    t
                    ring_acc
                    acc
                    visited
                    visited_ring_acc
            )
        )
    )
let find_rings (g: graph) (source: cart) =
    find_rings g [(cart_to_string source)] [] []  (Hashtbl.create (module String)) (Hashtbl.create (module String)) |> List.rev

let distance_to_up (c: cart): float =
    let x = x c |> Float.of_int in
    let y = y c |> Float.of_int in
    match x, y with
    | 0.0, y when y > 0.0 -> 0.0
    | 0.0, y when y < 0.0 -> Float.pi
    | x, 0.0 when x > 0.0 -> Float.pi /. 2.0
    | x, 0.0 when x < 0.0 -> 1.5 *. Float.pi
    | x, y when x > 0.0 && y > 0.0 -> Float.atan (x /. y)
    | x, y when x > 0.0 && y < 0.0 -> Float.pi +. Float.atan (x /. y)
    | x, y when x < 0.0 && y < 0.0 -> Float.pi +. Float.atan (x /. y)
    | _ -> 2.0 *. Float.pi +. Float.atan (x /. y)

let reorient_single ((x, y): cart) ((x', y'): cart): cart = (x' - x, y - y')
let reorient (pivot: cart) (rings: ring list): ring list =
    rings
    |> List.map ~f:(fun ring -> List.map ring ~f:(fun c -> reorient_single pivot c))

let normalize_single ((x, y): cart) ((x', y'): cart): cart = (x' + x, y - y')
let normalize (pivot: cart) (rings: ring list): ring list =
    rings
    |> List.map ~f:(fun ring -> List.map ring ~f:(fun c -> normalize_single pivot c))

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
    match part with
    | One -> (
        match Hashtbl.to_alist g |> List.max_elt ~compare:(fun (a, ac) (b, bc) -> (List.length ac) - (List.length bc)) with
        | Some (a, ac) -> printf "%s [%d]\n" a (List.length ac)
        | None -> failwith "No max found"
    )
    | Two -> (
        printf "(%s) => %s => (%s)\n" ((11,12) |> cart_to_string) ((11,12) |> reorient_single (11,13) |> cart_to_string) ((11,12) |> reorient_single (11,13) |> normalize_single (11,13) |> cart_to_string);
        printf "(%s) => %s => (%s)\n" ((10,12) |> cart_to_string) ((10,12) |> reorient_single (11,13) |> cart_to_string) ((10,12) |> reorient_single (11,13) |> normalize_single (11,13) |> cart_to_string);
        printf "(%s) => %s => (%s)\n" ((13,11) |> cart_to_string) ((13,11) |> reorient_single (11,13) |> cart_to_string) ((13,11) |> reorient_single (11,13) |> normalize_single (11,13) |> cart_to_string);
        match Hashtbl.to_alist g |> List.max_elt ~compare:(fun (a, ac) (b, bc) -> (List.length ac) - (List.length bc)) with
        | Some (a, ac) -> (
            printf "Started out with %d points\n" (List.length points);
            let station = cart_of_string a in
            printf "Station: (%s)\n" a;
            let sorted =
            find_rings g station
                |> reorient station
                (* |> List.iter ~f:(fun ring -> printf "ring: "; List.iter ring ~f:(fun s -> printf "(%s) " (cart_to_string s)); printf "\n") *)
                |> List.map ~f:(fun l -> List.sort l ~compare:(fun a b -> (distance_to_up a -. distance_to_up b) *. 1000.0 |> Int.of_float))
                |> normalize station in
            printf "Ended up with %d rings, total %d\n" (List.length sorted) (List.fold ~init:0 sorted ~f:(fun acc a -> acc + List.length a));
            let ring = ref 0 in
            let idx = ref 0 in
            List.iteri sorted ~f:(fun ringi r -> List.iteri r ~f:(fun i a -> if a = (8,2) then (ring := ringi; idx := i)));
            printf "Ring 0 has %d members\n" (List.hd_exn sorted |> List.length);
            printf "8,2 is in ring %d, idx %d\n" !ring !idx;
            let positions = Hashtbl.create (module String) in
            let pos = ref 1 in
            List.iter sorted ~f:(fun ring -> List.iter ring ~f:(fun c -> (
                let _ = Hashtbl.add positions (cart_to_string c) !pos in ();
                incr pos
            )));
            show_distances points positions;
            let pos = ref 1 in
            List.iter sorted ~f:(fun ring -> List.iter ring ~f:(fun c -> (
                printf "%d. (%s) normalized (%s): %f\n" !pos (c |> cart_to_string) (c |> reorient_single station |> cart_to_string) (distance_to_up (reorient_single station c));
                incr pos
            )));
            sorted
                |> List.iter ~f:(fun ring -> printf "ring: "; List.iter ring ~f:(fun c -> printf "(%s) " (cart_to_string c)); printf "\n")
        )
        | None -> failwith "No max found"
    )
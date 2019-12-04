open Core

type dir = Up | Down | Left | Right
type step = dir * int
type cart = int * int
type weighted_intersection = cart * int
type line = cart * cart * int * dir (* A * B * prev distance *)

let fst4 a = let (a, _, _, _) = a in a
let snd4 a = let (_, a, _, _) = a in a
let trd4 a = let (_, _, a, _) = a in a
let frt4 a = let (_, _, _, a) = a in a

let to_cart_delta (s: step): cart =
    match s with
    | Up, n -> (0, n)
    | Down, n -> (0, -n)
    | Left, n -> (-n, 0)
    | Right, n ->  (n, 0)

let (<->) (a: cart) (b: cart): int =
    let (x, y), (x', y') = a, b in
    (abs (x' - x)) + (abs (y' - y))

let (<>+) (a: cart) (b: cart): cart =
    let (x, y), (x', y') = a, b in
    (x + x'), (y + y')

let miny (a: cart) (b: cart): cart =
    let (x, y), (x', y') = a, b in
    if y < y' then a else b
let maxy (a: cart) (b: cart): cart =
    let (x, y), (x', y') = a, b in
    if y > y' then a else b
let minx (a: cart) (b: cart): cart =
    let (x, y), (x', y') = a, b in
    if x < x' then a else b
let maxx (a: cart) (b: cart): cart =
    let (x, y), (x', y') = a, b in
    if x > x' then a else b

let x (c: cart): int = fst c
let y (c: cart): int = snd c
let dist (l: line): int = trd4 l
let ldir (l: line): dir = frt4 l

let min_dist (p: cart) (ps: cart list): int =
    match ps
    (* |> List.filter ~f:(fun (a, b) -> match a, b with | 0, _ -> false | _, 0 -> false | _ -> true) *)
    |> List.map ~f:(fun p' -> (p, p'))
    |> List.map ~f:(fun (p, p') -> p <-> p')
    |> List.min_elt ~compare:(fun a b -> a - b) with
    | Some d -> d
    | None -> 0

let min_dist_weighted (p: cart) (ps: weighted_intersection list): int =
    match ps
    |> List.map ~f:(fun p' -> (snd p'))
    (* |> List.map ~f:(fun (p, (p', w)) -> p <-> p') *)
    |> List.min_elt ~compare:(fun a b -> a - b) with
    | Some d -> d
    | None -> 0

exception UnknownDirtection of char
let parse_steps txt: step list =
    txt
    |> String.split ~on:','
    |> List.map ~f:(fun sop -> String.strip sop)
    |> List.map ~f:(fun sop -> (
        let dir = match String.get sop 0 with
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | c -> raise (UnknownDirtection c) in
        let mag =
            String.sub sop 1 (String.length sop - 1)
            |> Int.of_string in
        (dir, mag)
    ))

let rec parse_lines (steps: step list) (lines: line list) (start: cart) (prev_dist: int): line list =
    match steps with
    | h :: t -> (
        let delta = to_cart_delta h in
        let lend = start <>+ delta in
        parse_lines t ((start, lend, prev_dist, (fst h)) :: lines) lend (prev_dist + (abs (snd h)))
    )
    | [] -> lines
let parse_lines (steps: step list): line list =
    List.rev (parse_lines steps [] (0, 0) 0)

exception IntersectionException
let intersect (a: line) (b: line): weighted_intersection option =
    let vert l = (x (fst4 l)) = (x (snd4 l)) in
    let hori l = (y (fst4 l)) = (y (snd4 l)) in
    let check verta horib =
        let (va, vb, _, _), (ha, hb, _, _) = verta, horib in
        let x' = x va in (* 2 *)
        let y' = y ha in (* 0 *)
        (x' > min (x ha) (x hb) && (* 2 > 0 *)
        x' < max (x ha) (x hb)) && (* 2 < 4 *)
        (y' > min (y va) (y vb) && (* 0 > -2 *)
        y' < max (y va) (y vb)) (* 0 < 2 *)
    in
    let vert_off vert_dir pt b = match vert_dir with
        | Up -> miny (fst4 b) (snd4 b) <-> pt
        | Down -> maxy (fst4 b) (snd4 b) <-> pt
        | _ -> raise IntersectionException
    in
    let hori_off hori_dir pt a = match hori_dir with
        | Right -> minx (fst4 a) (snd4 a) <-> pt
        | Left -> maxx (fst4 a) (snd4 a) <-> pt
        | _ -> raise IntersectionException
    in
    match vert a, hori b, check a b, check b a with
    | true, true, true, false-> (
        let vert_dist = dist a in
        let hori_dist = dist b in
        let pt = (x (fst4 a), y (fst4 b)) in
        let vert_dir = ldir a in
        let hori_dir = ldir b in
        Some (pt, (vert_dist + hori_dist + (vert_off vert_dir pt a) + (hori_off hori_dir pt b)))
    )
    | false, false, false, true -> (
        let vert_dist = dist b in
        let hori_dist = dist a in
        let pt = (x (fst4 b), y (fst4 a)) in
        let vert_dir = ldir b in
        let hori_dir = ldir a in
        Some (pt, (vert_dist + hori_dist + (vert_off vert_dir pt b) + (hori_off hori_dir pt a)))
    )
    | _ -> None

let intersect (ls1: line list) (ls2: line list): weighted_intersection list =
    List.cartesian_product ls1 ls2
    |> List.map ~f:(fun (a, b) -> intersect a b)
    |> List.filter ~f:(fun c -> match c with | Some c -> true | _ -> false)
    |> List.map ~f:(fun c -> match c with | Some c -> c | _ -> ((0, 0), 0))


exception FormatException of string
let solve () =
    match In_channel.read_lines "inputs/3.txt" with
    | line1 :: line2 :: [] -> (
        let line1 = line1 |> parse_steps |> parse_lines in
        let line2 = line2 |> parse_steps |> parse_lines in
        let intersections = intersect line1 line2 in
        printf "%d\n" (min_dist_weighted (0, 0) intersections)
    )
    | _ -> raise (FormatException "improper format")
    
open Core

let num_steps = 1000

type pos = int * int * int
let (++) ((x, y, z): pos) ((x', y', z'): pos) = (x + x', y + y', z + z')
let abs_sum ((x, y, z): pos) = Int.abs x + Int.abs y + Int.abs z

type moon = pos * pos
let moon_to_string ((x, y, z), (vx, vy, vz): moon): string =
    sprintf "(%d, %d, %d) : (%d, %d, %d)" x y z vx vy vz
let p (m: moon) = fst m
let v (m: moon) = snd m
let pot_e m = m |> p |> abs_sum
let kin_e m = m |> v |> abs_sum
let tot_e m = pot_e m * kin_e m
let gravity ((x, y, z), (vx, vy, vz): moon) ((x', y', z'), (vx', vy', vz'): moon): moon =
    let act a b =
        match a, b with
        | a, b when a > b -> -1
        | a, b when a < b ->1
        | _, _ -> 0 in
    ((x, y, z), (vx + act x x', (vy + act y y'), (vz + act z z')))
let apply_vel ((x, y, z), (vx, vy, vz): moon): moon =
    ((x + vx), (y + vy), (z + vz)), (vx, vy, vz)

let rec simulate (moons: moon list) steps_left: moon list =
    match steps_left with
    | 0 -> moons
    | _ -> (
        let moons = moons |> Array.of_list in
        let moon_indices = moons |> Array.mapi ~f:(fun i _ -> i) in
        Array.cartesian_product moon_indices moon_indices
        |> Array.iter ~f:(fun (i, i') -> (
            let m = moons.(i) in
            let m' = moons.(i') in
            moons.(i) <- gravity m m'
        ));
        let moons =
            moons
            |> Array.to_list
            |> List.map ~f:apply_vel in
        simulate moons (steps_left -1)
    )
let simulate (moons: moon list): moon list = simulate moons num_steps


let parse (data: string): moon =
    match String.slice data 1 (String.length data - 1)
        |> String.split ~on:','
        |> List.map ~f:String.strip
        |> List.map ~f:(String.split ~on:'=')
        |> List.map ~f:(fun a -> a |> List.tl_exn |> List.hd_exn)
        |> List.map ~f:Int.of_string with
    | [x; y; z] -> ((x, y, z), (0, 0, 0))
    | _ -> failwith "Malformed moon data"


let solve () =
    let total_e = In_channel.read_lines "inputs/12.txt"
    |> List.map ~f:parse
    |> simulate
    |> List.fold ~init:0 ~f:(fun acc m -> acc + (tot_e m)) in
    printf "%d\n" total_e;
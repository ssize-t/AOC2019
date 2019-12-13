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

let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)
 
let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let lcm (steps: (int * int * int)) =
    let (sx, sy, sz) = steps in
    [sx;sy;sz]
    |> List.fold ~init:1 ~f:(fun acc v -> lcm acc v)

let prev_x = Hashtbl.create (module String)
let prev_y = Hashtbl.create (module String)
let prev_z = Hashtbl.create (module String)

let rec simulate (moons: moon list) steps num_steps: int =
    let pos_to_string pos = pos |> List.map ~f:Int.to_string |> String.concat ~sep:"," in 
    let step_done s =
        match s with
        | (sx, sy, sz) when sx <> 0 && sy <> 0 && sz <> 0 -> true
        | _ -> false in
    match steps with
    | s' when step_done s'-> lcm steps
    | s' ->
        let (sx,sy,sz) = steps in
        let sx' = match sx with
        | 0 -> (
            let xs = List.map moons ~f:(fun ((x,_,_),(vx,_,_)) -> pos_to_string [x;vx]) |> String.concat ~sep:"|" in
            match Hashtbl.find prev_x xs with
            | Some n -> num_steps
            | None -> let _ = Hashtbl.add prev_x xs (num_steps) in 0
        )
        | _ -> sx in
        let sy' = match sy with
        | 0 -> (
            let ys = List.map moons ~f:(fun ((_,y,_),(_,vy,_)) -> pos_to_string [y;vy]) |> String.concat ~sep:"|" in
            match Hashtbl.find prev_y ys with
            | Some n -> num_steps
            | None -> let _ = Hashtbl.add prev_y ys (num_steps) in 0
        )
        | _ -> sy in
        let sz' = match sz with
        | 0 -> (
            let zs = List.map moons ~f:(fun ((_,_,z),(_,_,vz)) -> pos_to_string [z;vz]) |> String.concat ~sep:"|" in
            match Hashtbl.find prev_z zs with
            | Some n -> num_steps
            | None -> let _ = Hashtbl.add prev_z zs (num_steps) in 0
        )
        | _ -> sz in
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
        simulate moons (sx',sy',sz') (num_steps + 1)
let simulate (moons: moon list): int =
    simulate moons (0,0,0) 0


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
    let moons = In_channel.read_lines "inputs/12.txt"
        |> List.map ~f:parse in
    let num_steps = simulate moons in
    printf "%d\n" num_steps
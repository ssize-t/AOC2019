open Core

type part = | One | Two
let part = Two

exception IllegalFormat of string
exception NoPathFound
exception NoSourceFound
let solve () =
    let orbits = Hashtbl.create (module String) in
    In_channel.read_lines "inputs/6.txt"
    |> List.map ~f:(fun l -> (
        match String.split ~on:')' l with
        | a :: b :: [] -> a, b
        | _ -> raise (IllegalFormat l)
    ))
    |> List.iter ~f:(fun (around, orbiting) -> (
        match Hashtbl.find orbits around with
        | Some existing_orbiting -> Hashtbl.remove orbits around; Hashtbl.add_exn orbits around (orbiting :: existing_orbiting)
        | None -> Hashtbl.add_exn orbits around [orbiting]
    ));
    match part with
    | One -> (
        let visited = Hashtbl.create (module String) in
        let direct = ref 0 in
        let indirect = ref 0 in
        let current = ref [("COM", 0)] in
        while (match !current with
            | [] -> false
            | (h, depth) :: t -> (
                match Hashtbl.find visited h with
                | Some _ -> current := t; true
                | None -> (
                        Hashtbl.add_exn visited h true;
                        match Hashtbl.find orbits h with
                        | Some new_indirect -> (
                            let new_indirect = List.map new_indirect ~f:(fun t -> (t, depth + 1)) in
                            let new_orbit_count = List.length new_indirect in
                            direct := (!direct + new_orbit_count);
                            indirect := (!indirect + (depth * new_orbit_count));
                            current := t @ new_indirect;
                            true
                        )
                        | None -> true
                )
            )) do
            ()
        done;
        printf "%d" (!direct + !indirect)
    )
    | Two -> (
        let visited = Hashtbl.create (module String) in
        let total_hops = ref 0 in
        let current = ref [("YOU", 0)] in
        while (match !current with
            | [] -> raise NoPathFound
            | (h, num_hops) :: t -> (
                match Hashtbl.find visited h with
                | Some _ -> current := t; true
                | None -> (
                        Hashtbl.add_exn visited h true;
                        match (
                            let backref = ref [] in
                            Hashtbl.iter_keys orbits ~f:(fun k -> (
                                match Hashtbl.find_exn orbits k |> List.filter ~f:(fun v -> v = h) with
                                | [] -> ()
                                | _ -> backref := (k :: !backref)
                            ));
                            match Hashtbl.find orbits h with
                            | Some forwardref -> forwardref @ !backref
                            | None -> !backref
                        ) with
                        | [] -> true
                        | next_hops -> (
                            let next_hops = List.map next_hops ~f:(fun t -> (t, num_hops + 1)) in
                            match (List.filter next_hops ~f:(fun h -> (fst h) = "SAN")) with
                            | [] -> current := (t @ next_hops); true
                            | (_, hops) :: _ -> total_hops := hops - 2; false
                        )
                )
            )) do
            ()
        done;
        printf "%d" !total_hops
    )
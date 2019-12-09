open Core
open Aoc

let solve num =
    match num with
    | 1 -> P1.solve ()
    | 2 -> P2.solve ()
    | 3 -> P3.solve ()
    | 4 -> P4.solve ()
    | 5 -> P5.solve ()
    | 6 -> P6.solve ()
    | 7 -> P7.solve ()
    | 8 -> P8.solve ()
    | 9 -> P9.solve ()
    | _ -> printf "Not solved yet, sorry :-)\n"

let () =
    let open Command.Param in
    Command.basic
        ~summary:"Solve an Advent of Code 2019 puzzle"
        (Command.Param.map (anon ("puzzle number" %: int)) ~f:(fun num ->
            (fun () -> solve num)))
    |> Command.run
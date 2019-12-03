open Core

let fuel_req mass = mass / 3 - 2 

let rec fuel_req_2 mass fuel =
    match fuel_req mass with
    | fuel' when fuel' <= 0 -> fuel
    | fuel' -> fuel_req_2 fuel' (fuel + fuel')

let fuel_req_2 mass = fuel_req_2 mass 0

let solve () =
    In_channel.read_lines "inputs/1.txt"
    |> List.fold ~init:0 ~f:(fun total line -> 
        let mass = Int.of_string line in
        total + fuel_req_2 mass)
    |> printf "%d\n"
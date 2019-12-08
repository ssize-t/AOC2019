open Core

let layer_width = 25
let layer_height = 6

let rec split_layer data row rows width height =
    match data, width, height with
    | _, _, 0 -> (data, List.rev rows)
    | _, 0, _ -> split_layer data [] ((List.rev row) :: rows) layer_width (height - 1)
    | h :: t, _, _ -> split_layer t (h :: row) rows (width - 1) height
    | _, _, _ -> failwith "Malformed layer data"

let rec split_layers data layers =
    let data', layer = split_layer data [] [] layer_width  layer_height in
    match data' with
    | [] -> layers
    | _ -> split_layers data' (layer :: layers)

let split_layers data = split_layers data [] |> List.rev

let solve () =
    match In_channel.read_all "inputs/8.txt"
    |> String.to_list
    |> List.map ~f:(fun a -> String.of_char a |> Int.of_string)
    |> split_layers
    |> List.min_elt ~compare:(fun a b -> (
        let a = List.concat a in
        let b = List.concat b in
        (List.filter a ~f:(fun v -> v = 0) |> List.length) -
        (List.filter b ~f:(fun v -> v = 0) |> List.length)
    )) with
    | Some min_layer -> (
        let min_layer = List.concat min_layer in
        let num1 = List.filter min_layer ~f:(fun v -> v = 1) |> List.length in
        let num2 = List.filter min_layer ~f:(fun v -> v = 2) |> List.length in
        printf "%d\n" (num1 * num2)
    )
    | None -> failwith "No min 0 layer found"
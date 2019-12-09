open Core
open ANSITerminal

type part = One | Two
let part = Two 

let layer_width = 25
let layer_height = 6

type color = Black | White | Transparent

let color_of_int i =
    match i with
    | 0 -> Black
    | 1 -> White
    | 2 -> Transparent
    | _ -> failwith "Unknown color"

let print_color color =
     match color with
    | Black -> printf [black; on_black] "□□"
    | White -> printf [white; on_white] "□□"
    | Transparent -> printf [] "□□"

let rec show_image (image: color list list) = 
    match image with
    | (h :: t) :: t' -> (
        print_color h;
        show_image (t :: t')
    )
    | [] :: t -> printf [] "\n"; show_image t
    | [] -> ()

let rec split_layer data row rows width height =
    match data, width, height with
    | _, _, 0 -> (data, List.rev rows)
    | _, 0, _ -> split_layer data [] ((List.rev row) :: rows) layer_width (height - 1)
    | h :: t, _, _ -> split_layer t (h :: row) rows (width - 1) height
    | _, _, _ -> failwith "Malformed layer data"

let rec split_layers data layers =
    let data', layer = split_layer data [] [] layer_width  layer_height in
    match data' with
    | [] -> layer :: layers
    | _ -> split_layers data' (layer :: layers)

let split_layers data = split_layers data [] |> List.rev

let rec project_pixel (pixel_layers: color list) =
    match pixel_layers with
    | Black :: _ -> Black
    | White :: _ -> White
    | Transparent :: t -> project_pixel t
    | [] -> Transparent
let project_pixel pixels = pixels |> List.map ~f:color_of_int |> project_pixel

let rec project_layers (layers: int list list list) (row_acc: color list) (image_acc: color list list) =
    match layers with
    | [] :: _ -> List.rev image_acc
    | _ -> (
        let rows = List.map layers ~f:List.hd_exn in
        let rest_layers = List.map layers ~f:List.tl_exn in
        match rows with
        | [] :: _ -> (
            let new_image_acc = (List.rev row_acc) :: image_acc in
            project_layers rest_layers [] new_image_acc
        )
        | _ -> (
            let head_pixels = List.map rows ~f:List.hd_exn in
            let rest_rows = List.map rows ~f:List.tl_exn in
            let projected = project_pixel head_pixels in
            let new_row_acc = projected :: row_acc in
            let new_layers = match List.zip rest_rows rest_layers with
                | Ok ll -> ll |> List.map ~f:(fun (row, layer) -> row :: layer)
                | _ -> failwith "Failed to project layers" in
            project_layers new_layers new_row_acc image_acc
        )
    )
let project_layers layers = project_layers layers [] []

let rec flatten_layers layers image =
    match layers with
    | [] :: _ -> List.rev image
    | _ -> (
        let pixel_layers = List.map layers ~f:List.hd_exn in
        let t = List.map layers ~f:List.tl_exn in
        let pixel_row = List.map pixel_layers ~f:project_pixel in
        flatten_layers t ((List.rev pixel_row) :: image)
    )
let flatten_layers layers = flatten_layers layers []

let solve () =
    let layers = In_channel.read_all "inputs/8.txt"
    |> String.to_list
    |> List.map ~f:(fun a -> String.of_char a |> Int.of_string)
    |> split_layers in
    match part with
    | One -> (
        match layers
            |> List.min_elt ~compare:(fun a b -> (
                let a = List.concat a in
                let b = List.concat b in
                (List.filter a ~f:(fun v -> v = 0) |> List.length) -
                (List.filter b ~f:(fun v -> v = 0) |> List.length))) with
        | Some min_layer -> (
            let min_layer = List.concat min_layer in
            let num1 = List.filter min_layer ~f:(fun v -> v = 1) |> List.length in
            let num2 = List.filter min_layer ~f:(fun v -> v = 2) |> List.length in
            printf [] "%d\n" (num1 * num2)
        )
        | None -> failwith "No min 0 layer found"
    )
    | Two -> (
        layers
        |> project_layers
        |> show_image
    )
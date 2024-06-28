open! Core

module Position = struct
  module T = struct
    type t =
      { row : int
      ; col : int
      ; position_char : char
      }
    [@@deriving compare, sexp, hash]
  end

  include Comparable.Make (T)
  include T
end

type t = { vertices : Position.Set.t }

let create_graph input_file =
  let maze_lines = In_channel.read_lines (File_path.to_string input_file) in
  let vertices_L =
    List.concat_mapi maze_lines ~f:(fun y line ->
      let line = String.to_list line in
      List.filter_mapi line ~f:(fun x position_char ->
        match position_char with
        | '#' -> None
        | _ -> Some { Position.row = y; col = x; position_char }))
  in
  let maze = { vertices = Position.Set.of_list vertices_L } in
  let start =
    Set.find maze.vertices ~f:(fun position ->
      Char.equal position.position_char 'S')
  in
  match start with
  | None -> maze, { Position.row = 0; col = 1; position_char = 'S' }
  | Some start -> maze, start
;;

let rec dfs (position : Position.t) maze visited : Position.t list =
  (* print_endline " "; print_endline (Char.to_string
     position.position_char); *)
  (* print_endline (Printf.sprintf "%d , %d" position.row position.col); *)
  Hash_set.add visited position;
  match position.position_char with
  | 'E' -> [ position ]
  | _ ->
    (* Hash_set.iter visited ~f:(fun pos -> print_endline (Printf.sprintf "%d
       , %d" pos.row pos.col)); *)
    let valid_neighbors =
      Set.filter maze.vertices ~f:(fun neighbor ->
        (Int.equal (position.row - 1) neighbor.row
         && Int.equal position.col neighbor.col)
        || (Int.equal (position.row + 1) neighbor.row
            && Int.equal position.col neighbor.col)
        || (Int.equal position.row neighbor.row
            && Int.equal (position.col + 1) neighbor.col)
        || (Int.equal position.row neighbor.row
            && Int.equal (position.col - 1) neighbor.col))
      |> Set.filter ~f:(fun next_position ->
        not
          (Hash_set.exists visited ~f:(fun visited_position ->
             Int.equal next_position.row visited_position.row
             && Int.equal next_position.col visited_position.col)))
    in
    (* Set.iter valid_neighbors ~f:(fun pos -> print_endline (Printf.sprintf
       "%d , %d Is a valid neighbor of %d , %d" pos.row pos.col position.row
       position.col)); *)
    (match Set.is_empty valid_neighbors with
     | true -> []
     | false ->
       let neighbor_list = Set.elements valid_neighbors in
       let next =
         List.concat_map neighbor_list ~f:(fun neighbor ->
           dfs neighbor maze visited)
       in
       (match next with [] -> [] | _ -> [ position ] @ next))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let graph, start = create_graph input_file in
        let visited = Hash_set.create (module Position) in
        let solution = dfs start graph visited in
        List.iter solution ~f:(fun position ->
          print_endline (Printf.sprintf "%d , %d" position.row position.col))

      (* List.find_map *)]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;

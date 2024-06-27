open! Core

module Position = struct

  module T = struct
    type t = {
      row : int
      ; col : int
      ; position_char : char
    } [@@deriving compare, sexp, hash]
  end

  include Comparable.Make (T)
  include T
end 

type t =  
  { length : int
  ; height : int
  ; vertices : Position.Set.t
  } 

let create_graph input_file = 

  let maze_lines = In_channel.read_lines ( File_path.to_string input_file) in
  
  let vertices_L= List.concat_mapi maze_lines ~f:(fun y line ->
    
    let line = String.to_list line in

    List.filter_mapi line ~f:(fun x position_char ->
      match position_char with 
      | '#' -> None
      | _ -> Some {Position.row = y ; col = x; position_char = position_char}

    )) in

    let maze = {length = 0; height = 0; vertices = (Position.Set.of_list vertices_L)} in
    let start =  Set.find maze.vertices ~f:(fun position -> Char.equal position.position_char 'S') in

    maze, start

;;

let rec dfs (position : Position.t) maze visited : Position.t list= 

  let valid_neighbors = Set.filter maze.vertices ~f:(fun neighbor -> 
   (Int.equal position.row (neighbor.row + 1) && Int.equal position.col (neighbor.col))
    || (Int.equal position.row (neighbor.row - 1) && Int.equal position.col (neighbor.col))
    || (Int.equal position.row (neighbor.row) && Int.equal position.col (neighbor.col - 1))
    || (Int.equal position.row (neighbor.row) && Int.equal position.col (neighbor.col + 1)))  in
  
  match Set.is_empty valid_neighbors with 
  | true -> ()
  | false ->
(*     
    Set.exists visited ~f:(fun pos -> Int.equal pos.row neighbor.row && Int.equal pos.col neighbor.col) in
  

        Set.map valid_neighbors ~f:(fun neighbor -> 
            match Char.equal neighbor.position_char 'E' with 
            | _ -> []))

        
            Set.map valid_neighbors ~f:(fun neighbor -> 
              match Char.equal neighbor.position_char 'E' with 
              | _ -> [])) *)

;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag "input" (required File_path.arg_type) ~doc:"FILE a file containing a maze"
      in
      fun () ->
        
        let graph, start = create_graph input_file in
        let visited = Position.Hash_set.create () in
        let solution = dfs start graph visited in
      
      ]
;;

let command = Command.group ~summary:"maze commands" [ "solve", solve_command ]

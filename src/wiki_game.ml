open! Core

(* type website = 
  {
    url : string;
    title : string
  } *)

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  
  let open Soup in

  (* Print the targets of all links. *)
  parse contents
  $$ "a[href*=/wiki/]" |> to_list
  |> List.map ~f:(fun a -> (R.attribute "href" a)) 
  |> List.filter ~f:( fun str ->  
    match Wikipedia_namespace.namespace str with
      | None -> true
      | _-> false) |> List.dedup_and_sort ~compare:String.compare;

  
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

let rec getWikiNetwork depthRemaining curFile how_to_fetch: (string * string) list = 
  let contents = File_fetcher.fetch_exn 
    (how_to_fetch) ~resource:curFile in

  let websiteConnections = get_linked_articles contents in
  List.concat_map ~f:(fun childWebsite -> 
    match depthRemaining with 
    | 0 -> []
    | _ -> [(curFile, childWebsite)] @  getWikiNetwork (depthRemaining - 1) childWebsite how_to_fetch) websiteConnections

;;
module G = Graph.Imperative.Graph.Concrete (String)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v =sprintf!"\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =

      let network =  (getWikiNetwork max_depth origin how_to_fetch ) in
      (* print_endline (Stdlib.string_of_int (List.length network));
      List.iter network ~f:(fun (from, too) -> 
        print_endline (String.concat ["from " ^ from ^ " to " ^ too])); *)
      let graph = G.create () in 
      List.iter network ~f:(fun (person1, person2) ->
        (* [G.add_edge] auomatically adds the endpoints as vertices in the
           graph if they don't already exist. *)
        G.add_edge graph person1 person2);
      Dot.output_graph
        (Out_channel.create (File_path.to_string output_file))
        graph;
  
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)

   (* let rec dfs (website : string) (destination : string) (connections : (string * string) list) (visited): string list= 

  Hash_set.add visited website;

  match website with 
  | destination -> [website]
  | _ ->
    
    let valid_neighbors = List.filter connections ~f:(fun (website1, website2) ->ignore website2; String.equal website1 website)
    |> List.filter ~f:(fun (website1, website2) -> 
      not (Hash_set.exists visited ~f:(fun (visited_website) -> 
        ignore website1;
        (String.equal visited_website website2))))in


    match List.is_empty valid_neighbors with 
    | true -> []
    | false ->
     
      let next = List.concat_map valid_neighbors ~f:(fun (website, neighbor) -> 
        ignore website; dfs neighbor destination connections visited)in

      match next with 
      | [] -> []
      | _ -> [website] @ next

;; *)

let correct_url url (how_to_fetch : File_fetcher.How_to_fetch.t) =
  match how_to_fetch with
  | Local _ -> url
  | Remote -> if not (String.is_prefix ~prefix:"https://" url) then "https://em.wikipedia.org" ^ url else url


  let rec bfs (destination : string) (connections : (string * string) list) (visited) (queue : string list) how_to_fetch: string list= 
    let cur_website = correct_url (List.hd_exn queue) how_to_fetch in
    Hash_set.add visited cur_website;

    print_endline (cur_website);
    match String.equal destination cur_website with 
    | true -> [cur_website]
    | _ ->
      
      let valid_neighbors = List.filter connections ~f:(fun (website1, website2) ->ignore website2; String.equal website1 cur_website)
      |> List.filter ~f:(fun (website1, website2) -> 
        not (Hash_set.exists visited ~f:(fun (visited_website) -> 
          ignore website1;
          (String.equal visited_website website2))))
        |> List.map ~f:(fun (website1, website2) -> ignore website1; website2) in

    
    let new_queue = queue @ valid_neighbors in
    match List.is_empty new_queue with 
    | true -> []
    | false ->
     
      let next =  bfs destination connections visited (List.tl_exn queue) how_to_fetch in

      match next with 
      | [] -> []
      | _ -> [cur_website] @ next

;;


(* let rec bfs graph website visited queue : string list =

  let connections = findFriends graph person in
  let newQueue =
    List.tl_exn queue
    @ List.filter connections ~f:(fun person ->
      match Hash_set.exists visited ~f:(String.equal person) with
      | true -> false
      | false ->
        Hash_set.add visited person;
        true)
  in
  match newQueue with
  | [] -> [ List.hd_exn queue ]
  | _ -> 
    let newPerson  =  List.hd_exn queue in
    [newPerson] @ bfs graph newPerson visited newQueue; *)
;;
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =

  let network =  getWikiNetwork max_depth origin how_to_fetch in
  let visited = String.Hash_set.create () in
  let queue = [origin] in
  let solution = bfs destination network visited queue how_to_fetch in
  List.iter solution ~f:print_endline ;

  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;



let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;

open Definition
open Constant
open GameType
open Util

(*Just a place where I'll be storing functions until I have a better place for
them (In the end, most of these functions will probably end up in util or 
gameHelper).*)

(******************************************************************************)
(** {2 Option utils}                                                          *)
(******************************************************************************)

let is_some o = not (is_none o)

(*****************************************************************************)
(* {list utils}                                                              *)
(*****************************************************************************)
(** Returns a list of indices of lst that satisfy the predicate p. *)
let list_indices_of (p : 'a -> bool) (lst : 'a list) : int list =
  let rec process l n =
    match l with
    | [] -> [] 
    | hd::tl -> 
      if (p hd) then n::(process tl (n+1))
      else process tl (n+1)
  in
  process lst 0

(*returns a list of elements from lst at the indices provided*)
let list_nths lst indices =
  if List.exists (fun i -> i < 0) indices then failwith "negative index"
  else
    let sorted_is = List.sort (compare) indices in
    let rec process lst sorted_is curr_i =
      match (lst, sorted_is) with
      | (_,[]) -> []
      | ([],_) -> failwith "invalid index"
      | (lst_hd::lst_tl, i_hd::i_tl) -> 
        if (curr_i = i_hd) then lst_hd::(process lst_tl i_tl (curr_i+1))
        else process lst_tl sorted_is (curr_i + 1)
    in process lst sorted_is 0

(*replaces the nth element of a list with e*)
let list_replace_nth lst n e =
  if n < 0 then failwith "negative index"
  else 
    let rec traverse lst n i =
      match lst with 
      | [] -> failwith "invalid index"
      | hd::tl ->  if n = i then e::tl else hd::(traverse tl n (i+1))
    in traverse lst n 0

(*****************************************************************************)
(* {point utils}                                                             *)
(*****************************************************************************)

(*returns all the points on the board except for those in pts*)
let point_complement (pts: point list) : point list = 
  let sorted_pts = List.sort (compare) pts in
  let rec loop sorted_pts n=
    if n<=53 then
      match sorted_pts with
      | [] -> n::(loop sorted_pts (n+1))
      | hd::tl -> 
        if hd = n then loop tl (n+1)
        else n::(loop sorted_pts (n+1))
    else []
  in
  loop sorted_pts 0

(*returns a list of points where settlements can still be built*)
let remaining_sett_locs (g: t) : point list = 
  let sett_locs = list_indices_of (is_some) g.board.structures.settlements in
  let adjacent_locs = List.map (adjacent_points) sett_locs in
  let invalid_locs = List.flatten (sett_locs::adjacent_locs) in
    point_complement invalid_locs

let all_lines = 
  let rec loop i stop =
    if i <= stop then 
      let f p = (i, p) in
        (List.map f (adjacent_points i))::(loop (i+1) stop)
    else [] 
  in
  List.flatten (loop 0 53)

(*returns a list of lines where roads can still be built
Note: if the line (p1, p2) is unoccupied, both (p1, p2) and (p2, p1) will 
appear in the list*)
let remaining_road_locs (g: t) : line list =
  let roads = g.board.structures.roads in
  let lines = List.map (fun (c, l) -> l) roads in
  let add_reverse (p1, p2) = [(p1, p2); (p2, p1)] in
  let lines_and_revs = List.flatten (List.map (add_reverse) lines) in
  let p e = not (List.mem e lines_and_revs) in
    List.filter p all_lines

(*returns a list of lines representing valid initial moves*)
let valid_initial_moves (g:t) : line list =
  let sett_locs = remaining_sett_locs g in
  let road_locs = remaining_road_locs g in
  let adjacent_road_locs sett = 
    let p (p1, p2) = (p1 = sett) in
      List.filter p road_locs
  in
  List.flatten (List.map adjacent_road_locs sett_locs)



(*returns a list of lines where a particular player can build roads*)


(*****************************************************************************)
(* {resource generation utils}                                                             *)
(*****************************************************************************)

let player_index (c: color) = 
  match c with
  | Blue -> 0
  | Red -> 1
  | Orange -> 2
  | White -> 3

let zero_cost_lst = 
  let zero_cost = (0, 0, 0, 0, 0) in
    [zero_cost; zero_cost; zero_cost; zero_cost]

(*adds the resources in cost to color c's entry in cost_list*)
let add_resources c cost cost_list =
  if List.length cost_list <> 4 then failwith "invalid cost list"
  else
    let rec traverse cost_list cost n i =
      match cost_list with
      | [] -> failwith "this shouldn't fail"
      | hd::tl -> 
        if n = i then (map_cost2 (+) hd cost)::tl
        else hd::(traverse tl cost n (i+1))
    in traverse cost_list cost (player_index c) 0

let supply_resources g (other_rs: cost list) index = 
  let (t,r) = List.nth g.board.map.hexes index in
  let resource = resource_of_terrain t in
  let resource_provided = 
    match resource with
    | None -> (0,0,0,0,0)
    | Some r -> single_resource_cost r
  in
  let corners = piece_corners index in
  let possible_setts = list_nths g.board.structures.settlements corners in
  let nearby_setts = List.filter is_some possible_setts in
  let f other_rs sett_opt = 
    let (c, sett) = get_some sett_opt in(*will only be called on Some options*)
      let mult = settlement_num_resources sett in
      let resources_provided = (map_cost (( * ) mult) resource_provided) in 
      add_resources c resources_provided other_rs
  in
  List.fold_left f zero_cost_lst nearby_setts


let resource_gen g roll =
  (*I may want to return un update game state instead of an inventory list
  indicating what was generated*) 
  let gen_hex_indices = 
    let p (t, r) = (r = roll) in 
      list_indices_of p g.board.map.hexes
  in List.fold_left (supply_resources g) zero_cost_lst gen_hex_indices


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
(** Returns the index of the first element satisfying the predicate. Not [[]]-safe *)
let list_indices_of (p : 'a -> bool) (lst : 'a list) : int list =
  let rec process l n =
    match l with
    | [] -> [] 
    | hd::tl -> 
      if (p hd) then n::(process tl (n+1))
      else process tl (n+1)
  in
  process lst 0


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


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

(** Unwraps Reveal (x) -> x, fails on Hidden *)
let get_reveal = function
  Reveal x -> x | Hidden i -> failwith "tried to get_reveal of cards"

(*****************************************************************************)
(* {list utils}                                                              *)
(*****************************************************************************)

(*returns a list consisting of n copies of e*)
let list_gen n e = 
  if n < 0 then failwith "n must be positive"
  else 
    let rec loop n =
      if n = 0 then [] else e::(loop (n-1))
    in
    loop n

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

(******************************************************************************)
(** {2 Hidden Utils}                                                          *)
(******************************************************************************)

let is_hidden = function
  | Hidden _ -> true
  | Reveal _ -> false

let is_revealed cards = not (is_hidden cards)

(*****************************************************************************)
(* {4-tuple utils}                                                           *)
(*****************************************************************************)

let map_4tuple f (a, b, c, d) = (f a, f b, f c, f d)

(** Maps a function across two 4-tuples *)
let map_4tuple2 f (a1, b1, c1, d1) (a2, b2, c2, d2) = 
  (f a1 a2, f b1 b2, f c1 c2, f d1 d2)

(*****************************************************************************)
(* {5-tuple utils}                                                           *)
(*****************************************************************************)
let fold_5tuple f acc (a,b,c,d,e) = List.fold_left f acc [a;b;c;d;e]

(******************************************************************************)
(** {Match utils}                                                             *)
(******************************************************************************)

let intersection_color i =
  match i with
  | None -> None
  | Some (color, _) -> Some color

let is_intersection_color color i = 
  match i with 
  | None -> false
  | Some (c,s) -> color = c

let is_intersection_town = function
  | Some (_, Town) -> true
  | _ -> false

let is_road_color color (c, l) = color = c

(* Returns the number of the number of victory points a settlement is worth*)
let settlement_num_vp (set : settlement) : int =
  match set with
    | Town -> cVP_TOWN
    | City -> cVP_CITY

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
let remaining_sett_locs (g: GameType.t) : point list = 
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
let remaining_road_locs (g: GameType.t) : line list =
  let roads = g.board.structures.roads in
  let lines = List.map (fun (c, l) -> l) roads in
  let add_reverse (p1, p2) = [(p1, p2); (p2, p1)] in
  let lines_and_revs = List.flatten (List.map (add_reverse) lines) in
  let p e = not (List.mem e lines_and_revs) in
    List.filter p all_lines

(*****************************************************************************)
(* {piece utils}                                                             *)
(*****************************************************************************)

(*returns all the points on the board except for those in pts*)
let piece_complement (pts: piece list) : piece list = 
  let sorted_pts = List.sort (compare) pts in
  let rec loop sorted_pts n=
    if n<=18 then
      match sorted_pts with
      | [] -> n::(loop sorted_pts (n+1))
      | hd::tl -> 
        if hd = n then loop tl (n+1)
        else n::(loop sorted_pts (n+1))
    else []
  in
  loop sorted_pts 0


(*****************************************************************************)
(* {move utils}                                                             *)
(*****************************************************************************)

(*returns a list of lines representing valid initial moves*)
let valid_initial_moves (g: GameType.t) : line list =
  let sett_locs = remaining_sett_locs g in
  let road_locs = remaining_road_locs g in
  let adjacent_road_locs sett = 
    let p (p1, p2) = (p1 = sett) in
      List.filter p road_locs
  in
  List.flatten (List.map adjacent_road_locs sett_locs)

let valid_robber_moves g : (piece * color option) list =
  let robber_position = g.board.robber in
  let valid_points = piece_complement [robber_position] in
  let adjacent_colors pt = 
    let corners = piece_corners pt in
    let possible_setts = list_nths g.board.structures.settlements corners in
    let f acc e = 
      let sett_color = intersection_color e in
        match sett_color with
        | None -> acc
        | c -> if List.mem c acc then acc else c::acc
    in
    let temp = List.fold_left f [] possible_setts in
      if temp = [] then [None] else temp
  in
  let f acc pt = 
    let c_opts = adjacent_colors pt in
    let f' acc c_opt = (pt, c_opt)::acc in
      (List.fold_left f' [] c_opts)@acc
  in 
    List.fold_left f [] valid_points


(*returns a list of roads that c can build*)
let c_buildable_roads (g: GameType.t) (c: color) : line list = 
  let rem_road_locs = remaining_road_locs g in
  let player_sett_locs = list_indices_of (is_intersection_color c) g.board.structures.settlements in
  (*look for remaining roads who share a point with a player sett location*)
  let p (p1, p2) = 
    let p' sett_loc = p1 = sett_loc || p2 = sett_loc in
      List.exists p' player_sett_locs
  in 
    List.filter p rem_road_locs

(*returns a list of towns that c can build*)
let c_buildable_towns (g: GameType.t) (c: color) : point list =
  let rem_sett_locs = remaining_sett_locs g in
  let player_road_locs = List.filter (is_road_color c) g.board.structures.roads
  in
  (*check for remaining settlements that are at the end of one the roads
  that the player controls. Note that rem_sett_locs automatically filters out
  the points adjacent to the player's city, along with any other locations
  that would violate the distance rule*)
  let p sett_loc = 
    let p' (_,(p1, p2)) = p1 = sett_loc || p2 = sett_loc in
      List.exists p' player_road_locs
  in
    list_indices_of p rem_sett_locs

(*returns a list of Cities that c can build--i.e. a list of points where c has
already established towns*)
let c_buildable_cities (g: GameType.t) (c: color) : point list = 
  let p i = is_intersection_town i && is_intersection_color c i in
  list_indices_of p g.board.structures.settlements

(******************************************************************************)
(** {game utils}                                                              *)
(******************************************************************************)

let set_blue g new_blue = {g with blue = new_blue}

let set_board g new_board = {g with board = new_board}

let set_structures g new_structures = 
  set_board g {g.board with structures = new_structures}

let set_settlements g new_setts =
  set_structures g {g.board.structures with settlements = new_setts}

let set_roads g new_roads =
  set_structures g {g.board.structures with roads = new_roads}

(******************************************************************************)
(** {build utils}                                                              *)
(******************************************************************************)

(*returns an updated game type that now has a road of color c that streches 
from p1 to p2
checks whether a road from p1 to p2 already exists, and throughs a failure if
one does*)
let build_road (g: GameType.t) (c: color) ((p1: point), (p2: point)) = 
  let p (color, (point1, point2)) = 
    (point1 = p1 && point2 = p2) || (point1 = p2 && point2 = p1) 
  in
  if List.exists p g.board.structures.roads then
    failwith "road already exists"
  else
    set_roads g ((c, (p1,p2))::g.board.structures.roads)

(*builds a town of color c at point p
fails if a town/city already exists at p*)
let build_town (g: GameType.t) (c: color) (p: point) =
  let intersections = g.board.structures.settlements in
  let intersection = List.nth intersections p in
  if is_some intersection then 
    failwith "settlement already exists"
  else 
    let new_settlements = 
      list_replace_nth g.board.structures.settlements p (Some (c, Town)) 
    in
      set_settlements g new_settlements

(*builds a city of color c at point p
succeeds only if a town exists at p and has the color c*)
let build_city (g: GameType.t) (c: color) (p: point) =
  let intersections = g.board.structures.settlements in
  let intersection = List.nth intersections p in
  let predicate = function
    | Some (sett_c, Town) -> sett_c = c
    | _ -> false
  in
  if not (predicate intersection) then 
    failwith "invalid build_city location"
  else 
    let new_settlements = 
      list_replace_nth g.board.structures.settlements p (Some (c, City)) 
    in
      set_settlements g new_settlements



(******************************************************************************)
(** {player utils}                                                            *)
(******************************************************************************)

(*a player record with no resources, cards, knights, nor trophies*)
let empty_pr =
  let inventory : resourcerecord = 
    { bricks = 0;
      wool = 0;
      ore = 0;
      grain = 0;
      lumber = 0
    }
  in
  { inventory = inventory;
    cards = Reveal [];
    knights = 0;
    longestroad = false;
    largestarmy = false;
    ratio = inventory
  }

type pr = playerrecord

let to_resource_rec (b, w, o, g, l) = 
  { bricks = b;
    wool = w;
    ore = o;
    grain = g;
    lumber = l;
  }

let to_ht_tuple (p: pr) : (hand * trophies) =
  let p_inv = p.inventory in
  let inv = (p_inv.bricks, p_inv.wool, p_inv.ore, p_inv.grain, p_inv.lumber) in
  ((inv, p.cards), (p.knights, p.longestroad, p.largestarmy))

let to_player_list (players : pr list) : player list =
  match players with
  | [blue; red; orange; white] ->
    let (bh, bt) = to_ht_tuple blue in
    let (rh, rt) = to_ht_tuple red in
    let (oh, ot) = to_ht_tuple orange in
    let (wh, wt) = to_ht_tuple white in
      [(Blue, bh, bt); (Red, rh, rt); (Orange, oh, ot); (White, wh, wt)]
  | _ -> failwith "invalid player record list"

let to_player_tuple (plist: player list) : (pr * pr * pr * pr) =
  (*qeb2: I'm not a huge fan of this implementation. It really ought
  to check whether the players have four distinct colors. Maybe I can do
  something clever with pattern matching to check that all four players are
  present.*)
  if List.length plist <> 4 then failwith "invalid player list"
  else 
    let f (color, (inventory, cards), (k, lr, la)) (blu, red, org, wht) =
      let (b, w, o, g, l) = inventory in 
      let new_record = 
        { inventory = 
          { bricks = b;
            wool = w;
            ore = o;
            grain = g;
            lumber = l
          };
          cards = cards;
          knights = k;
          longestroad = lr;
          largestarmy = la;
          ratio = 
          { bricks = 4;
            wool = 4;
            ore = 4;
            grain = 4;
            lumber = 4
          }
        }
      in
      match color with
      | Blue -> (new_record, red, org, wht)
      | Red ->  (blu, new_record, org, wht)
      | Orange -> (blu, red, new_record, wht)
      | White -> (blu, red, org, new_record)
    in 
    List.fold_right f plist (empty_pr, empty_pr, empty_pr, empty_pr) 

let set_inventory pr new_inv = {pr with inventory = new_inv}


let set_color g c new_c = match c with
  | Blue -> {g with blue = new_c}
  | Red -> {g with red = new_c}
  | White -> {g with white = new_c}
  | Orange -> {g with orange = new_c}

let set_cards pr new_cards = {pr with cards = new_cards}

let set_knights pr new_knights = {pr with knights = new_knights}

let set_longest_road pr new_lr = {pr with longestroad = new_lr}

let set_largestarmy pr new_la = {pr with largestarmy = new_la}

let set_ratio pr new_ratio = {pr with ratio = new_ratio}

let resource_rec_to_tuple r = (r.bricks, r.wool, r.ore, r.grain, r.lumber)

(*returns c's inventory as a 5-tuple--i.e. an inventory*)
let inv g c = 
  match c with 
  | Blue -> resource_rec_to_tuple g.blue.inventory
  | Red -> resource_rec_to_tuple g.red.inventory
  | Orange -> resource_rec_to_tuple g.orange.inventory
  | White -> resource_rec_to_tuple g.white.inventory


(*****************************************************************************)
(* {resource generation utils}                                               *)
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

(*returns a new game state where each player's resources have been appropriately
updated.*)
let resource_gen g roll =
  (*I may want to return un update game state instead of an inventory list
  indicating what was generated*) 
  let gen_hex_indices = 
    let p (t, r) = (r = roll) in 
      list_indices_of p g.board.map.hexes
  in 
  let new_resources = 
    List.fold_left (supply_resources g) zero_cost_lst gen_hex_indices
  in 
  let new_b_inv = map_cost2 (+) (inv g Blue) (List.nth new_resources 0) in
  let new_r_inv = map_cost2 (+) (inv g Red) (List.nth new_resources 1) in
  let new_o_inv = map_cost2 (+) (inv g Orange) (List.nth new_resources 2) in
  let new_w_inv = map_cost2 (+) (inv g White) (List.nth new_resources 3) in
    { board = g.board;
      blue = set_inventory g.blue (to_resource_rec new_b_inv);
      red = set_inventory g.red (to_resource_rec new_r_inv);
      orange = set_inventory g.orange (to_resource_rec new_o_inv);
      white = set_inventory g.white (to_resource_rec new_w_inv);
      turn = g.turn;
      next = g.next
    }

(*****************************************************************************)
(* {victory point utils}                                                     *)
(*****************************************************************************)
let calc_hand_vp cards =
    let value = function
      | VictoryPoint -> cVP_CARD
      | _ -> 0
    in
    let f acc e = acc + (value e) in
      List.fold_left f 0 cards 

let calc_vp (g: GameType.t) : (int * int * int * int) =
  let setts = g.board.structures.settlements in
  let players = to_player_list [g.blue; g.red; g.orange; g.white] in
  let calc_sett_vp = function
    | None -> (0, 0, 0, 0) 
    | Some (Blue, sett) -> (settlement_num_vp sett, 0, 0, 0)
    | Some (Red, sett) -> (0, settlement_num_vp sett, 0, 0)
    | Some (Orange, sett) -> (0, 0, settlement_num_vp sett, 0)
    | Some (White, sett) -> (0, 0, 0, settlement_num_vp sett)
  in 
  let sett_vps = 
    let f (vpb, vpr, vpo, vpw) e = 
      let (e_vpb, e_vpr, e_vpo, e_vpw) = calc_sett_vp e in
        (vpb + e_vpb, vpr + e_vpr, vpo + e_vpo, vpw + e_vpw)
    in
    List.fold_left f (0, 0, 0, 0) setts
  in
  let player_vps = 
    let player_vp (color,(_, cards),(_, lr, la)) = 
      let card_vp =
        match cards with
        | Hidden n -> failwith "this shouldn't fail"
        | Reveal cs -> calc_hand_vp cs
      in
      let lr_vp = if lr then cVP_LONGEST_ROAD else 0 in
      let la_vp = if la then cVP_LARGEST_ARMY else 0 in
      let tot_vp = card_vp + lr_vp + la_vp in
        match color with
        | Blue -> (tot_vp, 0, 0, 0)
        | Orange -> (0, tot_vp, 0, 0)
        | Red -> (0, 0, tot_vp, 0)
        | White -> (0, 0, 0, tot_vp)
    in
    let f acc e = map_4tuple2 (+) acc (player_vp e) in
      List.fold_left f (0,0,0,0) players
  in
  map_4tuple2 (+) sett_vps player_vps


(*****************************************************************************)
(* {validate move utils}                                                     *)
(*****************************************************************************)

let valid_trade_helper g (id, cost1, cost2) = 
    let rec_to_tuple r = (r.bricks, r.wool, r.ore, r.grain, r.lumber) in
    let inv g c = 
      match c with 
      | Blue -> rec_to_tuple g.blue.inventory
      | Red -> rec_to_tuple g.red.inventory
      | Orange -> rec_to_tuple g.orange.inventory
      | White -> rec_to_tuple g.white.inventory in
    let f_or_t (b1, b2, b3, b4, b5) = b1 && b2 && b3 && b4 && b5 in
    let owns g c t = f_or_t (map_cost2 (>=) (inv g c) t) in
    (owns g g.turn.active cost1) && (owns g id cost2)


(*****************************************************************************)
(* {potentially bad idea utils}                                              *)
(*****************************************************************************)

let ratio_helper (blue, red, orange, white) settlements ports =
  (* update player ratios based on settlements and ports *)
  let fold_help color ((i, acc) : (int * resourcerecord)) (inter : intersection) =
    let exist_helper (((p1 : int), (p2 : int)), _, _) : bool = 
      (p1 = i) || (p2 = i) in
    match inter with
    | Some (c,_ ) when c = color -> 
      begin
        if not (List.exists (exist_helper) ports) then (i + 1, acc)
        else let (l, ra, res) = (List.find (exist_helper) ports) in
        match res with
        | Any -> 
          begin
            i + 1, { bricks = (min ra acc.bricks);
              ore = (min ra acc.ore);
              wool = (min ra acc.wool);
              lumber = (min ra acc.lumber);
              grain = (min ra acc.grain) }
          end
        | PortResource Brick -> i + 1, {acc with bricks = (min ra acc.bricks);}
        | PortResource Ore -> i + 1, {acc with ore = (min ra acc.ore);}
        | PortResource Wool -> i + 1, {acc with wool = (min ra acc.wool);}
        | PortResource Lumber -> i + 1, {acc with lumber = (min ra acc.lumber);}
        | PortResource Grain -> i + 1, {acc with grain = (min ra acc.grain);}
      end
    | _ -> (i + 1, acc) in
  let blue_ratio = snd (List.fold_left (fold_help Blue) (0, blue.ratio) settlements) in
  let red_ratio = snd (List.fold_left (fold_help Red) (0, red.ratio) settlements) in
  let orange_ratio = snd (List.fold_left (fold_help Orange) (0, orange.ratio) settlements) in
  let white_ratio = snd (List.fold_left (fold_help White) (0, white.ratio) settlements) in
  ( 
    {blue with ratio = blue_ratio},
    {red with ratio = red_ratio},
    {orange with ratio = orange_ratio},
    {white with ratio = white_ratio}
  )

let state_of_game g =
  let hexes = g.board.map.hexes in
  let ports = g.board.map.ports in
  let structures = (g.board.structures.settlements,g.board.structures.roads) in
  let deck = g.board.deck in
  let discard = g.board.discard in
  let robber = g.board.robber in
  let plist = to_player_list [g.blue; g.red; g.orange; g.white] in
  let turn = g.turn in
  let next = g.next in
    (((hexes, ports), structures, deck, discard, robber), plist, turn, next)

let game_of_state ((map, structs, deck, discard, robber), plist, turn, next) =
  let (hexes, ports) =  map in
  let (settlements, roads) = structs in
  let board = 
    { map = {hexes = hexes; ports = ports};
      structures = {settlements = settlements; roads = roads};
      deck = deck;
      discard = discard;
      robber = robber
    } in
  let (blue, red, orange, white) = to_player_tuple plist in
  let (blue, red, orange, white) = ratio_helper (blue, red, orange, white) settlements ports in
  { board = board;
    blue = blue;
    red = red;
    orange = orange;
    white = white;
    turn = turn; 
    next = next
  }


(*****************************************************************************)
(* {trade utils}                                              *)
(*****************************************************************************)

let modify_resource g2 f resource inv =
    match resource with
    | Brick -> {inv with bricks = (f inv.bricks)}
    | Wool -> {inv with wool = (f inv.wool)}
    | Ore -> {inv with ore = (f inv.ore)}
    | Lumber -> {inv with lumber = (f inv.lumber)}
    | Grain -> {inv with lumber = (f inv.grain)} 

let player g c = 
    match c with
    | White -> g.white
    | Red -> g.red
    | Blue -> g.blue
    | Orange -> g.orange

(*****************************************************************************)
(* {card playing utils}                                                      *)
(*****************************************************************************)
let is_card_viable state color = function
  | Knight -> true
  | VictoryPoint -> false
  | RoadBuilding ->  
    List.length (c_buildable_roads (game_of_state state) color) > 0
  | YearOfPlenty -> true
  | Monopoly -> true


let viable_card_plays s c hand = List.filter (is_card_viable s c) hand



(*****************************************************************************)
(* {trade utils}                                                             *)
(*****************************************************************************)

(*returns potential 1-1 trades of Wool/Ore/Grain for Lumber*)
let lumber_trades my_c my_inv players =
  let players_with_lumber =
    let p (c, (i, _), _) = 
      let temp = (map_cost2 (>=) i (single_resource_cost Lumber)) in
        (c <> my_c) && (fold_5tuple (||) false temp)
    in
      List.filter p players
  in 
  let my_offers = 
    let i_have r = 
      fold_5tuple (&&) true (map_cost2 (>=) my_inv (single_resource_cost r))
    in
    let my_resources = List.filter i_have [Wool; Ore; Grain] in
    let f r = (single_resource_cost r, single_resource_cost Lumber) in
      List.map f my_resources
  in
  let f (color, _, _) = 
    let f' acc (c1, c2) = (color, c1, c2)::acc in
      List.fold_left f' [] my_offers
  in
    List.flatten (List.map f players_with_lumber)

(*returns potential 1-1 trades of Wool/Ore/Grain for Bricks*)
let brick_trades my_c my_inv players =
  let players_with_bricks =
    let p (c, (i, _), _) = 
      let temp = (map_cost2 (>=) i (single_resource_cost Brick)) in
        (c <> my_c) && (fold_5tuple (||) false temp)
    in
      List.filter p players
  in 
  let my_offers = 
    let i_have r = 
      fold_5tuple (&&) true (map_cost2 (>=) my_inv (single_resource_cost r))
    in
    let my_resources = List.filter i_have [Wool; Ore; Grain] in
    let f r = (single_resource_cost r, single_resource_cost Lumber) in
      List.map f my_resources
  in
  let f (color, _, _) = 
    let f' acc (c1, c2) = (color, c1, c2)::acc in
      List.fold_left f' [] my_offers
  in
    List.flatten (List.map f players_with_bricks)


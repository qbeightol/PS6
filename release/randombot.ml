open Definition
open Registry
open Constant
open Util
open MoreUtil
(*open Game*)

(** Give your bot a 2-20 character name. *)
let name = "randombot"

module Bot = functor (S : Soul) -> struct

let initialize () = ()

let handle_request (s : state) : move =
  let (b, p, t, n) = s in
  let (c, r) = n in
  match r with
  | InitialRequest -> 
    let move_opt = pick_random (valid_initial_moves (game_of_state s)) in
      if is_none move_opt then InitialMove(0, 0) (*this shouldn't happen*)
      else InitialMove (get_some move_opt)
  | RobberRequest -> 
    let move_opt = pick_random (valid_robber_moves (game_of_state s)) in
      if is_none move_opt then RobberMove (0, None) (*shouldn't happen*)
      else RobberMove (get_some move_opt)
  | DiscardRequest->
    let (b, w, o, g, l) = inv (game_of_state s) c in
    let mkd_inv = ((Brick, b), (Wool, w), (Ore, o), (Grain, g), (Lumber, l)) in
    let f acc (r, c) = (list_gen c r)::acc in
    let discard_list = List.flatten (fold_5tuple f [] mkd_inv) in
    let qty_to_discard = (b+w+o+g+l)/2 in
    let rec loop n d_list = 
      if n = 0 then [] 
      else 
        let (d, rem_d_list) = pick_one d_list in 
          d::(loop (n-1) rem_d_list)
    in
    let discards = loop qty_to_discard discard_list in
    let costs = List.map single_resource_cost discards in
    let tot_cost = List.fold_left (map_cost2 (+)) (0,0,0,0,0) costs in
      DiscardMove tot_cost
  | TradeRequest -> 
    (*ask to borrow alic's logic for checking whether a trade is valid*)
    let move_opt = pick_random [true; false] in
      if is_none move_opt then TradeResponse false (*shouldn't happen*)
      else TradeResponse (get_some move_opt)
  | ActionRequest -> 
    if is_none t.dicerolled then Action(RollDice) 
    else failwith "not implemented"

end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
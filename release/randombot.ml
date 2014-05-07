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
  let (b, ps, t, n) = s in
  let (my_c, r) = n in
  let (_, (my_inv, my_cards), _) = 
    let p (c, (_, cards), _) = c = my_c in
      List.find p ps
  in 
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
    let (b, w, o, g, l) = my_inv in
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
    (*ask to borrow alice's logic for checking whether a trade is valid*)
    let move_opt = pick_random [true; false] in
      if is_none move_opt then TradeResponse false (*shouldn't happen*)
      else TradeResponse (get_some move_opt)
  | ActionRequest -> 
    let roll =is_none t.dicerolled in
    let trade = not (t.tradesmade >= cNUM_TRADES_PER_TURN) in
    let l_trades = lumber_trades my_c my_inv ps in
    let l_trade_viable = (List.length l_trades) <> 0 in
    let trd_l = trade && (t.tradesmade = 0) && l_trade_viable in
    let b_trades = brick_trades my_c my_inv ps in
    let b_trade_viable = (List.length b_trades) <> 0 in
    let trd_b = trade && (t.tradesmade = 1) && b_trade_viable in
    let viable_cards = 
      if is_hidden my_cards then [] 
      else viable_card_plays s my_c (reveal my_cards)
    in
    let play_c = (not t.cardplayed) && List.length viable_cards <> 0 in
    let buy_r = fold_5tuple (&&) true (map_cost2 (>=) my_inv cCOST_ROAD) in
    let buy_t = fold_5tuple (&&) true (map_cost2 (>=) my_inv cCOST_TOWN) in
    let buy_cty = fold_5tuple (&&) true (map_cost2 (>=) my_inv cCOST_CITY) in
    let buy_crd = fold_5tuple (&&) true (map_cost2 (>=) my_inv cCOST_CARD) in
    match (roll,trd_l,trd_b,play_c,buy_t,buy_r,buy_cty,buy_crd) with
    | (true, _, _, _, _, _, _, _) -> Action RollDice
    | (_, true, _, _, _, _, _, _) -> 
      (*try to trade lumber*)
      let move_opt = pick_random (l_trades) in
        if is_none move_opt then Action EndTurn (*shouldn't happen*)
        else Action (DomesticTrade (get_some move_opt))
    | (_, _, true, _, _, _, _, _) -> 
      (*try to trade brick*)
      let move_opt = pick_random (b_trades) in
        if is_none move_opt then Action EndTurn (*shouldn't happen*)
        else Action (DomesticTrade (get_some move_opt))
    | (_, _, _, true, _, _, _, _) -> 
      (*play a card*)
      let card =
        let card_opt = pick_random viable_cards in
          get_some card_opt
      in
      begin
        match card with
        | Knight -> 
          let move_opt = pick_random (valid_robber_moves (game_of_state s)) in
            if is_none move_opt then 
              (*shouldn't happen*)
              Action (PlayCard (PlayKnight (0, None)))
            else 
              Action (PlayCard (PlayKnight (get_some move_opt)))
        | VictoryPoint -> Action EndTurn (*shouldn't happen*)
        | RoadBuilding -> 
          let poss_roads = c_buildable_roads (game_of_state s) my_c in
          let (l1, rem_poss_roads) = pick_one poss_roads in
          let rd1 = (my_c, l1) in
          let l2_opt = pick_random rem_poss_roads in
          let rd2_opt = 
            match l2_opt with 
            | None -> None
            | Some l2 -> Some (my_c, l2)
          in
            Action (PlayCard (PlayRoadBuilding (rd1, rd2_opt)))
        | YearOfPlenty ->
          let rs = [Brick; Wool; Ore; Grain; Lumber] in
          let r1 = get_some (pick_random rs) in
          let r2 = get_some (pick_random rs) in
            Action (PlayCard (PlayYearOfPlenty (r1, Some r2)))
        | Monopoly -> 
          let rs = [Brick; Wool; Ore; Grain; Lumber] in
          let r = get_some (pick_random rs) in
            Action (PlayCard (PlayMonopoly r))
      end
    | (_, _, _, _, true, _, _, _) -> 
      (*buy a town*)
      failwith "not implemented"
    | (_, _, _, _, _, true, _, _) -> 
      (*buy a road*)
      failwith "not implemented"
    | (_, _, _, _, _, _, true, _) -> 
      (*buy a city*)
      failwith "not implemented"
    | (_, _, _, _, _, _, _, true) -> 
      (*buy a card*)
      failwith "not implemented"
    | _ ->
      (*nothing left to do*)
      Action EndTurn


end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
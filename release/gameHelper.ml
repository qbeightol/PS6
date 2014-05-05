open Definition
open Constant
open Util
open Print
open GameType
open MoreUtil


(* checks to see if m is a valid move, returns true/false *)
let validmove g m = 

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
	(owns g g.turn.active cost1) && (owns g id cost2) in

	match snd (g.next) with
	| InitialRequest -> 
		begin
			match m with 
			| InitialMove l -> List.exists ((=) l) (valid_initial_moves g)
			| _ -> false
		end
	| RobberRequest ->
		begin
			match m with
			| RobberMove (p, c) -> (p >= 0) && (p < 19)
			| _ -> false
		end
	| DiscardRequest -> 
		begin
			match m with
			| DiscardMove c ->
				begin
					let c1 = sum_cost c in
					match g.turn.active with
					| Blue -> ((g.blue.inventory.bricks + g.blue.inventory.wool + 
							g.blue.inventory.lumber + g.blue.inventory.ore + 
							g.blue.inventory.grain)/2 = c1) && (c1 > 3)
					| Red -> ((g.red.inventory.bricks + g.red.inventory.wool + 
							g.red.inventory.lumber + g.red.inventory.ore + 
							g.red.inventory.grain)/2 = c1) && (c1 > 3)
					| Orange -> ((g.orange.inventory.bricks + g.orange.inventory.wool + 
							g.orange.inventory.lumber + g.orange.inventory.ore + 
							g.orange.inventory.grain)/2 = c1) && (c1 > 3)
					| White -> ((g.white.inventory.bricks + g.white.inventory.wool + 
							g.white.inventory.lumber + g.white.inventory.ore + 
							g.white.inventory.grain)/2 = c1) && (c1 > 3)
			  end
			| _ -> false
		end
	| TradeRequest ->
		begin
			match m with
			| TradeResponse b ->
			  begin 
			    match g.turn.pendingtrade with
			    | None -> false
			    | Some (id, cost1, cost2) -> valid_trade_helper g (id, cost1, cost2)
			  end
			| _ -> false
		end
	| ActionRequest -> 
		begin
			match m with
			| Action a ->
			  begin
			    match a with
			    | RollDice -> (g.turn.dicerolled = None)
			    | MaritimeTrade (sold, bought) -> failwith "how do i know which ratio to use?"
			    | DomesticTrade (id, cost1, cost2) -> 
			    		if g.turn.tradesmade >= cNUM_TRADES_PER_TURN then false else
			    		valid_trade_helper g (id, cost1, cost2)
			    | BuyBuild b -> 
			    	begin
			        let cost1 = (cost_of_build b) in
			        let rec_to_tuple r = (r.bricks, r.wool, r.ore, r.grain, r.lumber) in
							let inv g c = 
								match c with 
								| Blue -> rec_to_tuple g.blue.inventory
								| Red -> rec_to_tuple g.red.inventory
								| Orange -> rec_to_tuple g.orange.inventory
								| White -> rec_to_tuple g.white.inventory in
							let f_or_t (b1, b2, b3, b4, b5) = b1 && b2 && b3 && b4 && b5 in
							let owns g c t = f_or_t (map_cost2 (>=) (inv g c) t) in
							(owns g g.turn.active cost1) 
						end
			    | PlayCard pc -> (match pc with 
			  		| PlayKnight r -> true
			  		| _ -> (g.turn.dicerolled <> None))
			    | EndTurn -> (g.turn.dicerolled <> None)
			  end
			| _ -> false
		end


(*a player record with no resources, cards, knights, nor trophies*)
let empty_pr =
  let inventory : costrecord = 
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
  }

type pr = playerrecord

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
      let new_inventory = 
        { bricks = b;
          wool = w;
          ore = o;
          grain = g;
          lumber = l
        }
      in 
      let new_record = 
        { inventory = new_inventory;
          cards = cards;
          knights = k;
          longestroad = lr;
          largestarmy = la
        }
      in
      match color with
      | Blue -> (new_record, red, org, wht)
      | Red ->  (blu, new_record, org, wht)
      | Orange -> (blu, red, new_record, wht)
      | White -> (blu, red, org, new_record)
    in 
    List.fold_right f plist (empty_pr, empty_pr, empty_pr, empty_pr) 

	(* change 
    the board's robber location to p. Then remove a random resource from
    the player with color c_opt and give it to the active player *)
let robber_helper g (p, c_opt) = failwith "not implemented"

(*  I assume this involves subtracting c from the player
    who made the discard move. I'm not quite sure how you tell which player
    discarded, though *)
let discard_helper g c = failwith "not implemented"

(*If true, then conduct the trade (and don't conduct
    the trade if false). Then return control to the active player.*)
let trade_helper g b =
	match b with
	| false -> (* return g with new action request *) failwith "error"
	| true -> (* return g with players items exchanged and new action request *) failwith "error"

(*check that the player can conduct this trade. If so, take away 
r_sold from the active player and give them r_bought.*)
let maritime_helper g (r_sold, r_bought) = failwith "not implemented"

(*Send a trade request to other player*)
let domestic_helper g (other_player, active_player_cost, other_player_cost) =
        failwith "not implemented"

let buyBuild_helper g b =
  begin
    match b with
    | BuildRoad rd -> failwith "not implemented"
    | BuildTown pt -> failwith "not implemented"
    | BuildCity pt -> failwith "not implemented"
    | BuildCard -> failwith "not implemented"
  end

let playCard_helper g pc =
  begin
    match pc with
    | PlayKnight r -> failwith "not implemented"
    | PlayRoadBuilding (rd, rd_o) -> failwith "not implemented"
    | PlayYearOfPlenty (r, r_o) -> failwith "not implemented"
    | PlayMonopoly r -> failwith "not implemented"
  end 


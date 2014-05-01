open Definition
open Constant
open Util
open Print
open GameType


(* checks to see if m is a valid move, returns true/false *)
let validmove g m = 
	match snd (g.next) with
	| InitialRequest -> 
		begin
			match m with 
			| InitialMove (p1, p2) ->
				(List.mem p2 (adjacent_points p1)) && (List.mem p1 (adjacent_points p2))
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
			    | Some (id, (b, w, o, g, l), (b2, w2, o2, g2, l2)) -> failwith "doesnt type check"
			    (* begin
			      match id, g.turn.active with 
 			      | Blue,Red -> 
			      		(g.blue.inventory.bricks >= b2) && (g.red.inventory.bricks >= b1)
			      		(g.blue.inventory.wool >= w2) && (g.red.inventory.wool >= w1)
			      		(g.blue.inventory.ore >= o2) && (g.red.inventory.ore >= o1)
			      		(g.blue.inventory.grain >= g2) && (g.red.inventory.grain >= g1)
			      		(g.blue.inventory.lumber >= l2) && (g.red.inventory.lumber >= l1)
			      	| Red,Blue ->
			      		(g.red.inventory.bricks >= b2) && (g.blue.inventory.bricks >= b1)
			      		(g.red.inventory.wool >= w2) && (g.blue.inventory.wool >= w1)
			      		(g.red.inventory.ore >= o2) && (g.blue.inventory.ore >= o1)
			      		(g.red.inventory.grain >= g2) && (g.blue.inventory.grain >= g1)
			      		(g.red.inventory.lumber >= l2) && (g.blue.inventory.lumber >= l1)
			      | _ -> failwith "error"
			    end *)
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
			    | MaritimeTrade (sold, bought) -> failwith "doesnt type check"
(* 			    | DomesticTrade (id, cost1, cost2) -> 
			        (id.inventory.bricks >= cost2.bricks) &&
			        (id.inventory.wool >= cost2.wool) &&
			        (id.inventory.ore >= cost2.ore) &&
			        (id.inventory.grain >= cost2.grain) &&
			        (id.inventory.lumber >= cost2.lumber) &&
			        (g.inventory.bricks >= cost1.bricks) &&
			        (g.inventory.wool >= cost1.wool) &&
			        (g.inventory.ore >= cost1.ore) &&
			        (g.inventory.grain >= cost1.grain) &&
			        (g.inventory.lumber >= cost1.lumber)
			    | BuyBuild b -> 
			        let c = (cost_of_build b) in
			        (g.inventory.bricks >= c.bricks) &&
			        (g.inventory.wool >= c.wool) &&
			        (g.inventory.ore >= c.ore) &&
			        (g.inventory.grain >= c.grain) &&
			        (g.inventory.lumber >= c.lumber) *)
			    | PlayCard pc -> (match pc with 
			  		| PlayKnight r -> true
			  		| _ -> (g.turn.dicerolled <> None))
			    | EndTurn -> (g.turn.dicerolled <> None)
			    | _ -> false
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

let to_player_tuple (plist: player list) : (pr * pr * pr * pr) =
  match plist with
  | _::_::_::[_] ->
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
  | _ -> failwith "invalid player list"

	(* change 
    the board's robber location to p. Then remove a random resource from
    the player with color c_opt and give it to the active player *)
let robber_helper (p, c) = failwith "not implemented"

(*  I assume this involves subtracting c from the player
    who made the discard move. I'm not quite sure how you tell which player
    discarded, though *)
let discard_helper c = failwith "not implemented"

(*If true, then conduct the trade (and don't conduct
    the trade if false). Then return control to the active player.*)
let trade_helper g b = failwith "not implemented"

(*check that the player can
        conduct this trade. If so, take away r_sold from the active 
        player and give them r_bought.*)
let maritime_helper (r_sold, r_bought) = failwith "not implemented"

(*Send a trade request to other player*)
let domestic_helper (other_player, active_player_cost, other_player_cost) =
        failwith "not implemented"

let buyBuild_helper b =
  begin
    match b with
    | BuildRoad rd -> failwith "not implemented"
    | BuildTown pt -> failwith "not implemented"
    | BuildCity pt -> failwith "not implemented"
    | BuildCard -> failwith "not implemented"
  end

let playCard_helper pc =
  begin
    match pc with
    | PlayKnight r -> failwith "not implemented"
    | PlayRoadBuilding (rd, rd_o) -> failwith "not implemented"
    | PlayYearOfPlenty (r, r_o) -> failwith "not implemented"
    | PlayMonopoly r -> failwith "not implemented"
  end 
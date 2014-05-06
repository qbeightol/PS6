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
			    | MaritimeTrade (sold, bought) ->
			    	begin 
			    	  let player = 
			    	  	match g.turn.active with
				    	  | Blue -> g.blue
				    	  | White -> g.white
				    	  | Red -> g.red
				    	  | Orange -> g.orange in
			    		let ratio, owns = 
			    			match sold with
				    		| Wool -> player.ratio.wool, player.inventory.wool
				    		| Brick -> player.ratio.bricks, player.inventory.bricks
				    		| Ore -> player.ratio.ore, player.inventory.ore
				    		| Grain -> player.ratio.grain, player.inventory.grain
				    		| Lumber -> player.ratio.lumber, player.inventory.lumber in
				    	(ratio <= owns)
						end
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

let present_player_info active_color player_color player =
  { inventory = player.inventory;
    cards = 
      if active_color = player_color then player.cards 
      else hide player.cards;
    knights = player.knights;
    longestroad = player.longestroad;
    largestarmy = player.largestarmy;
    ratio = player.ratio
  }

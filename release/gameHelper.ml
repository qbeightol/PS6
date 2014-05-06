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
let robber_helper g (p, c_opt) = 
	match c_opt with 
	| None -> {g with next = (g.turn.active, ActionRequest);
										board = {g.board with robber = p}}
	| Some c -> 
			let lst = [Brick; Wool; Ore; Lumber; Grain] in
			let n = Random.int (List.length lst) in 
			let res = List.nth lst n in
			let g2 = { g with next = (g.turn.active, ActionRequest);
												board = {g.board with robber = p} } in
			match g.turn.active with
			| Blue -> 
				begin 
					match c with 
					| White -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks - 1)} } }
							| Wool -> { g2 with
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain - 1)} } }
						end
					| Red -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain - 1)} } }
						end
					| Orange -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain - 1)} } }
						end
					| Blue -> g2
				end
			| White -> 
				begin 
					match c with 
					| Blue -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks - 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks + 1)} } }
							| Wool -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool - 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool + 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore - 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore + 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber - 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber + 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain - 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain + 1)} } }
						end
					| Red -> 
						begin
							match res with
							| Brick -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain - 1)} } }
						end
					| Orange -> 
						begin
							match res with
							| Brick -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain - 1)} } }
						end
					| White -> g2
				end
			| Red -> 
				begin 
					match c with 
					| White -> 
						begin
							match res with
							| Brick -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain + 1)} };
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain - 1)} } }
						end
					| Blue -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks - 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks + 1)} } }
							| Wool -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool - 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool + 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore - 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore + 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber - 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber + 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain - 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain + 1)} } }
						end
					| Orange -> 
						begin
							match res with
							| Brick -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool - 1)} } }
							| Ore -> { g2 with
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain + 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain - 1)} } }
						end
					| Red -> g2
				end
			| Orange -> 
							begin 
					match c with 
					| Blue -> 
						begin
							match res with
							| Brick -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with bricks = (g.blue.inventory.bricks - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks + 1)} } }
							| Wool -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with wool = (g.blue.inventory.wool - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool + 1)} } }
							| Ore -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with ore = (g.blue.inventory.ore - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore + 1)} } }
							| Lumber -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with lumber = (g.blue.inventory.lumber - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber + 1)} } }
							| Grain -> { g2 with 
									blue = {g.blue with inventory = 
										{g.blue.inventory with grain = (g.blue.inventory.grain - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain + 1)} } }
						end
					| Red -> 
						begin
							match res with
							| Brick -> { g2 with 
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with bricks = (g.red.inventory.bricks - 1)} } }
							| Wool -> { g2 with 
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with wool = (g.red.inventory.wool - 1)} } }
							| Ore -> { g2 with 
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with ore = (g.red.inventory.ore - 1)} } }
							| Lumber -> { g2 with 
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with lumber = (g.red.inventory.lumber - 1)} } }
							| Grain -> { g2 with 
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain + 1)} };
									red = {g.red with inventory = 
										{g.red.inventory with grain = (g.red.inventory.grain - 1)} } }
						end
					| White -> 
						begin
							match res with
							| Brick -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with bricks = (g.white.inventory.bricks - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with bricks = (g.orange.inventory.bricks + 1)} } }
							| Wool -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with wool = (g.white.inventory.wool - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with wool = (g.orange.inventory.wool + 1)} } }
							| Ore -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with ore = (g.white.inventory.ore - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with ore = (g.orange.inventory.ore + 1)} } }
							| Lumber -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with lumber = (g.white.inventory.lumber - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with lumber = (g.orange.inventory.lumber + 1)} } }
							| Grain -> { g2 with 
									white = {g.white with inventory = 
										{g.white.inventory with grain = (g.white.inventory.grain - 1)} };
									orange = {g.orange with inventory = 
										{g.orange.inventory with grain = (g.orange.inventory.grain + 1)} } }
						end
					| Orange -> g2
				end
			

(*  Subtracting c from the current player *)
let discard_helper g (b, w, o, gr, l) = 
	match g.turn.active with 
	| Blue -> 
		{ g with next = (Blue, ActionRequest);
			blue = {g.blue with inventory = { 
					bricks = (g.blue.inventory.bricks - b);
					wool = (g.blue.inventory.wool - w);
					ore = (g.blue.inventory.ore - o);
					grain = (g.blue.inventory.grain - gr);
					lumber = (g.blue.inventory.lumber - l)
				} } }
	| White ->
		{ g with next = (White, ActionRequest);
			white = {g.white with inventory = { 
					bricks = (g.white.inventory.bricks - b);
					wool = (g.white.inventory.wool - w);
					ore = (g.white.inventory.ore - o);
					grain = (g.white.inventory.grain - gr);
					lumber = (g.white.inventory.lumber - l)
				} } }
	| Red ->
		{ g with next = (Red, ActionRequest);
			red = {g.red with inventory = { 
					bricks = (g.red.inventory.bricks - b);
					wool = (g.red.inventory.wool - w);
					ore = (g.red.inventory.ore - o);
					grain = (g.red.inventory.grain - gr);
					lumber = (g.red.inventory.lumber - l)
				} } }
	| Orange ->
		{ g with next = (Orange, ActionRequest);
			orange = {g.orange with inventory = { 
					bricks = (g.orange.inventory.bricks - b);
					wool = (g.orange.inventory.wool - w);
					ore = (g.orange.inventory.ore - o);
					grain = (g.orange.inventory.grain - gr);
					lumber = (g.orange.inventory.lumber - l)
				} } }


(*If true, then conduct the trade (and don't conduct
    the trade if false). Then return control to the active player.*)
let trade_helper g b =
	match b with
	| false -> {g with next = (g.turn.active, ActionRequest)}
	| true -> (* return g with items exchanged in pendingTrade and new action request *)
		(* {g with next = (g.turn.active, ActionRequest)} *) failwith "too much matching?"

(*take away r_sold from the active player and give them r_bought.*)
let maritime_helper g (r_sold, r_bought) = failwith "too much matching?"
(* 	match g.turn.active with
	| Blue -> {g with blue = {g.blue with inventory = 
					{ g.blue.inventory with 
						bricks = (g.blue.inventory.r_sold - g.blue.ratio.r_sold);
						wool = (g.blue.inventory.r_bought + 1) }
			}	}
	| Red -> {g with red = {g.red with inventory = 
					{ g.red.inventory with 
						r_sold = (g.red.inventory.r_sold - g.red.ratio.r_sold);
						r_bought = (g.red.inventory.r_bought + 1) }
			}	}
	| Orange -> {g with orange = {g.orange with inventory = 
					{ g.orange.inventory with 
						r_sold = (g.orange.inventory.r_sold - g.orange.ratio.r_sold);
						r_bought = (g.orange.inventory.r_bought + 1) }
			}	}
	| White -> {g with white = {g.white with inventory = 
					{ g.white.inventory with 
						r_sold = (g.white.inventory.r_sold - g.white.ratio.r_sold);
						r_bought = (g.white.inventory.r_bought + 1) }
			}	} *)

(* next = trade request to other player, increase tradesmade, update pendingtrade *)
let domestic_helper g (other_player, active_player_cost, other_player_cost) =
        failwith "error"
        

(* buy build, update victory points and trophies *)
let buyBuild_helper g b =
(*   let (br, w, o, gr, l) = cost_of_build b in *)
  match b with
  | BuildRoad rd -> (* update inventory - cost of build, next is ActionRequest, 
  		check length of longest road, award trophy and 2 VPs if necessary, check if winner *)
			let lines = remaining_road_locs g in
			if lines = lines then g else g (* fill in *)
  | BuildTown pt -> (* update inventory - cost of build,
  		award 1 VP, check if winner else ActionRequest *)
  		failwith "not implemented"
  | BuildCity pt -> (* update inventory - cost of build, next is ActionRequest,
  		award 2 VPs, check if winner *)
  		failwith "not implemented"
  | BuildCard -> (* update inventory - cost of build, add to active player's cards *)
  		failwith "not implemented"

(* play card, update victory points and trophies *)
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

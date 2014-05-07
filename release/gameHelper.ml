open Definition
open Constant
open Util
open Print
open GameType
open MoreUtil


(* checks to see if m is a valid move, returns true/false *)
let validmove g m = 

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
			| RobberMove (p, c) -> List.mem (p,c) (valid_robber_moves g)
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
							(owns g g.turn.active cost1) && (
							match b with 
							| BuildRoad (c, l) -> List.mem l (c_buildable_roads g c)
		          | BuildTown point -> List.mem point (c_buildable_towns g g.turn.active)
		          | BuildCity point -> List.mem point (c_buildable_cities g g.turn.active) 
							| BuildCard -> (List.length (get_reveal g.board.deck)) > 0 )
						end
			    (* need to also check that player has card in hand *)
			    | PlayCard pc -> 
			    		(* let crd = get_reveal player.cards in (List.mem pc crd) && ---why doesn't it work...*)

			    (not g.turn.cardplayed) && (match pc with 
			  		| PlayKnight r -> true
			  		| PlayRoadBuilding (a, x) -> not g.turn.cardplayed 
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

	let rob g2 robber_color victim_color = 
    let r_inv = inv g2 robber_color in
    let v_inv = inv g2 victim_color in
    let v_resources = resources_in_inv v_inv in
      if v_resources = [] then
        g2
      else 
    	  let resource_opt = pick_random v_resources in
    	  let resource = get_some resource_opt in
        let r_inv_rec = to_resource_rec r_inv in
        let v_inv_rec = to_resource_rec v_inv in
    	  let new_r_inv = modify_resource g2 succ resource (r_inv_rec) in
    	  let new_v_inv = modify_resource g2 pred resource (v_inv_rec) in
    	  let new_r = set_inventory (player g2 robber_color) new_r_inv in
    	  let new_v = set_inventory (player g2 victim_color) new_v_inv in
    	  let g' = set_color g2 robber_color new_r in 
    		  set_color g' victim_color new_v 
  in
	let game2 = {g with next = (g.turn.active, ActionRequest); 
			                board = {g.board with robber = p}} 
  in
  	match c_opt with
  	| None -> game2
  	| Some c -> rob game2 (g.turn.active) c



let initial_helper g (pt1, pt2) = 
	let (build_color, _) = g.next in
	let g_with_road = build_road g build_color (pt1, pt2) in
	let g_with_town = build_town g_with_road build_color pt1 in
	let num_of_setts = count_sett_locs g_with_town in
		match num_of_setts with 
		| 1 | 2 | 3 -> 
			(*pass control to the next player*)
      let nxt_c = next_turn build_color in
  			{g_with_town with turn = {g.turn with active = nxt_c};
                          next = (nxt_c, InitialRequest)}
		| 4 -> 
			(*pass control to the current player*)
			g_with_town
		| 5 | 6 | 7  -> 
			(*pass control to the next player in reverse order*)
      let nxt_c = prev_turn build_color in
        {g_with_town with turn = {g.turn with active = nxt_c};
                          next = (nxt_c, InitialRequest)}
		| _ -> 
			(*serve an action request to the first player*)
			{g_with_town with next = (build_color, ActionRequest)}

(*  Subtracting c from the current player *)
let discard_helper g (b, w, o, gr, l) = 
  let next = 
    match discard_player g with
    | None -> (g.turn.active, RobberRequest)
    | Some color -> (color, DiscardRequest)
  in
	match g.turn.active with 
	| Blue -> 
		{ g with next = next;
			blue = {g.blue with inventory = { 
					bricks = (g.blue.inventory.bricks - b);
					wool = (g.blue.inventory.wool - w);
					ore = (g.blue.inventory.ore - o);
					grain = (g.blue.inventory.grain - gr);
					lumber = (g.blue.inventory.lumber - l)
				} } }
	| White ->
		{ g with next = next;
			white = {g.white with inventory = { 
					bricks = (g.white.inventory.bricks - b);
					wool = (g.white.inventory.wool - w);
					ore = (g.white.inventory.ore - o);
					grain = (g.white.inventory.grain - gr);
					lumber = (g.white.inventory.lumber - l)
				} } }
	| Red ->
		{ g with next = next;
			red = {g.red with inventory = { 
					bricks = (g.red.inventory.bricks - b);
					wool = (g.red.inventory.wool - w);
					ore = (g.red.inventory.ore - o);
					grain = (g.red.inventory.grain - gr);
					lumber = (g.red.inventory.lumber - l)
				} } }
	| Orange ->
		{ g with next = next;
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
	| false -> 
    {g with turn = {g.turn with pendingtrade = None}; 
							next = (g.turn.active, ActionRequest)}
	| true -> (* return g with items exchanged in pendingTrade and new action request *)
		(* {g with next = (g.turn.active, ActionRequest)} *) 
    let (id_color, a_cost, id_cost) =get_some g.turn.pendingtrade in
    let a_color = g.turn.active in
    let a_inv = inv g a_color in
    let id_inv = inv g id_color in
    let new_a_inv = map_cost2 (+) (map_cost2 (-) a_inv a_cost) id_cost in
    let new_id_inv = map_cost2 (+) (map_cost2 (-) id_inv id_cost) a_cost in
    let new_a = set_inventory (player g a_color) (to_resource_rec new_a_inv) in
    let new_id = set_inventory (player g id_color) (to_resource_rec new_id_inv) in
    let g' = set_color g a_color new_a in
    let g'' = set_color g' id_color new_id in
      {g'' with turn = {g.turn with pendingtrade = None}; 
                next = (a_color, ActionRequest)}


(*take away r_sold from the active player and give them r_bought.*)
let maritime_helper g (r_sold, r_bought) = 
	let player' = match g.turn.active with
		| Blue -> g.blue
		| White -> g.white
		| Red -> g.red
		| Orange -> g.orange in
	let (b, w, o, gr, l) = resource_rec_to_tuple (player'.ratio) in
	let ns = match r_sold with
		| Brick -> b
		| Wool -> w
		| Ore -> o
		| Grain -> gr
		| Lumber -> l in
	let mar g2 act_color r_sold r_bought nsold = 
	  let a_inv = inv g act_color in
	  let id_inv = inv g act_color in
	  let new_a_inv = modify_resource g (fun x -> x - nsold) r_sold (to_resource_rec a_inv) in
	  let new_id_inv = modify_resource g succ r_bought (to_resource_rec id_inv) in
	  let new_a = set_inventory (player g act_color) new_a_inv in
	  let new_id = set_inventory (player g act_color) new_id_inv in
	  let g' = set_color g act_color new_a in 
		set_color g' act_color new_id in

	let gm = mar g g.turn.active r_sold r_bought ns in
	{gm with next = (g.turn.active, ActionRequest)}

(* next = trade request to other player, increase tradesmade, update pendingtrade *)
let domestic_helper g (other_player, active_player_cost, other_player_cost) =
	{g with next = (other_player, TradeRequest); 
		turn = {g.turn with tradesmade = (g.turn.tradesmade + 1);
							pendingtrade = (Some (other_player, active_player_cost, other_player_cost))
						}
	}
        

(* buy build, update victory points and trophies *)
let buyBuild_helper g b =
(*   let (br, w, o, gr, l) = cost_of_build b in *)
  let (br, w, o, gr, l) = cost_of_build b in
	let b_road g2 act_color res nres = 
	  let a_inv = inv g act_color in
	  let new_a_inv = modify_resource g (fun x -> x - nres) res (to_resource_rec a_inv) in
	  let new_a = set_inventory (player g act_color) new_a_inv in
	  set_color g act_color new_a in 
	let gb = b_road g g.turn.active Brick br in 
	let gw = b_road gb g.turn.active Wool w in 
	let go = b_road gw g.turn.active Ore o in 
	let gl = b_road go g.turn.active Lumber l in 
	let gg = b_road gl g.turn.active Grain gr in 
	let g' = { gg with next = (g.turn.active, ActionRequest)} in

  match b with
  | BuildRoad rd -> (* build the road on board
  		check length of longest road, award trophy and 2 VPs if necessary, check if winner *)
		begin 
			g'
		end 

  | BuildTown pt -> (* build town on board, award 1 VP, check if winner else ActionRequest *)
		begin 
			g'
		end 

  | BuildCity pt -> (* build city on board, award 2 VPs, check if winner *)
  		failwith "not implemented"
  | BuildCard -> (* add to active player's cards (random card), remove card from board deck *)
  		(* let rancrd = pick_random (get_reveal g.board.deck) in 
  		let crdlst = get_reveal player.cards in
  		set_color g' g.turn.active (player with cards = randcrd::crdlst) *)
			failwith "not compiling"

(* play card, update victory points and trophies *)
let playCard_helper g pc =
	let g' = {g with next = (g.turn.active, ActionRequest); 
			turn = {g.turn with cardplayed = true}} in
  begin
    match pc with
    | PlayKnight r -> g' (* check victory points and trophy, update player's cards *)
    | PlayRoadBuilding (rd, rd_o) -> failwith "not implemented" (* build road, VPs, trophy, update cards*)
    | PlayYearOfPlenty (r, r_o) -> failwith "not implemented" (* update player's inventory and cards *)
    | PlayMonopoly r -> failwith "not implemented" (* update all inventory, update player's cards *)
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

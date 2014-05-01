open Definition
open Constant
open Util
open Print

type game = state (*keep here?*)

(* checks to see if m is a valid move, returns true/false *)
let validmove m = 
	match snd (g.next) with
	| InitialRequest -> 
		if m = InitialMove (p1, p2) 
		then (List.mem p2 (adjacent_points p1)) && (List.mem p1 (adjacent_points p2))
		else false
	| RobberRequest ->
		if m = RobberMove (p, c)
		then (p >= 0) && (p < 19)
		else false
	| DiscardRequest ->
		if m = DiscardMove (b, w, o, g, l) 
		then let c = (b + w + o + g + l) in
		  (((g.inventory.bricks + g.inventory.wool + g.inventory.ore + 
		  g.inventory.grain + g.inventory.lumber) / 2) = c) && (c > 3)
		else false
	| TradeRequest ->
		if m = TradeResponse b 
		then 
		  begin 
		    match g.turn.pendingtrade with
		    | None -> false
		    | Some (id, cost1, cost2) -> 
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
		  end
		else false
	| ActionRequest -> 
		if m = Action a 
		then 
		  begin
		    match a with
		    | RollDice -> (g.turn.diceRolled = None)
		    | MaritimeTrade (sold, bought) -> failwith "not yet implemented"
		    | DomesticTrade (id, cost1, cost2) -> 
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
		        (g.inventory.lumber >= c.lumber)
		    | Playcard pc -> if (pc <> PlayKnight r) then (g.turn.diceRolled <> None)
		    | EndTurn -> (g.turn.diceRolled <> None)
		  end
		else false


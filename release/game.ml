open Definition
open Constant
open Util
open Print



type game = state

(*Is anything else worth including/calculating?*)


let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())
(*Change to game_of_state(gen_random_initial_state ())*)

let handle_move g m = 
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
            | Some (id, cost1, cost2) -> (id.inventory = cost2) && (g.inventory = cost1)
          end
        else false
    | ActionRequest -> 
        if m = Action a 
        then 
          begin
            match a with
            | RollDice -> (g.turn.diceRolled = None)
            | MaritimeTrade mt -> 
            | DomesticTrade dt ->
            | BuyBuild b ->
            | Playcard pc ->
            | EndTurn -> (g.turn.diceRolled <> None)
          end
        else false
		

  (*I'm guessing we'll want to keep track of the request that m corresponds to.
  Maybe that's in turn/next*)
  match m with
  | InitialMove l -> (*Check for valid placement. If the placement is valid, 
  update the map. Otherwise, insert a minimum viable move?*)
  | RobberMove r ->
    match r with (p * c_opt) -> (*Assuming p is a valid location, change 
    the board's robber location to p. Then remove a random resource from
    the player with color c_opt and give it to the active player*)
  | DiscardMove c -> (*I assume this involves subtracting c from the player
    who made the discard move. I'm not quite sure how you tell which player
    discarded, though*)
  | TradeResponse b -> (*If true, then conduct the trade (and don't conduct
    the trade if false). Then return control to the active player.*)
  | Action a ->
    begin
      match a with
      | RollDice ->
        (*Generate a random dice roll. If seven, handle discards, then ask the
        active player where they want to place the robber*)
      | MaritimeTrade mt -> 
        match mt with (r_sold, r_bought) -> (*check that the player can
        conduct this trade. If so, take away r_sold from the active 
        player and give them r_bought.*)
      | DomesticTrade t ->
        match t with (other_player, active_player_cost, other_player_cost) ->
        (*Send a trade request to other player*)
      | BuyBuild b ->
        begin
          match b with
          |BuildRoad rd -> 
          | BuildTown pt -> 
          | BuildCity pt ->
          | BuildCard ->
        end
      | Playcard pc -> 
        begin
          match pc with
          | PlayKnight r ->
          | PlayRoadBuilding (rd, rd_o) ->
          | PlayYearOfPlenty (r, r_o) -> 
          | PlayMonopoly r -> 
        end 
      | EndTurn ->
        (*Pass control to the next player*)
        let (b, p, t, _) = g in
        let next_color = next_turn (t.active) in
        (b, p, new_turn (next_color), (next_color, ActionRequest))

      
    end



let presentation g = failwith "Were not too much to pay for birth."

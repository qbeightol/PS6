open Definition
open Constant
open Util
open Print
open GameType
open GameHelper
open MoreUtil



type game = t (*can I remove this declaration, or will the compiler freak out?*)

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
    }
  in
  let (blue, red, orange, white) = to_player_tuple plist in
    { board = board;
      blue = blue;
      red = red;
      orange = orange;
      white = white;
      turn = turn; 
      next = next
    }


let init_game () = game_of_state (gen_initial_state())
(*Change to game_of_state(gen_random_initial_state ())*)



(*cleaned up version of handle_move. Old code can be found below*)
let handle_move g m = 
  let move = 
    if validmove g m then m
    else 
      let (c, req) = g.next in
        match req with
        | InitialRequest -> 
          InitialMove (get_some (pick_random (valid_initial_moves g)))
        | RobberRequest -> failwith "not implemented"
        | DiscardRequest -> failwith "not implemented"
        | TradeRequest -> failwith "not implemented"
        | ActionRequest ->
          if is_none g.turn.dicerolled then Action(RollDice) 
                                       else Action(EndTurn)
  in
  let updated_game = 
    match move with
    | InitialMove l -> failwith "not implemented"
    | RobberMove (p, c_opt) -> failwith "not implemented"
    | DiscardMove c -> failwith "not implemented"
    | TradeResponse b -> failwith "not implemented"
    | Action a ->
      begin
        match a with
        | RollDice -> 
          let roll = random_roll () in failwith "not implemented"
          (*go through the tiles on the board and find the tiles whose rolls 
          match the current roll. 

          I'll probably want to use list_indices_of with the predicate
          p (t, r) = (r = roll) 

          then I'll want to find the players next to each tile, and reward
          them the appropriate amound and kind of resources. 

          So I'll probably want to use List.fold. And the accumulator probably
          should be a list of tuples (at least, that'll make the construction 
          and deconstruction of the data a little easier).

          Go through the tiles
          find the locations of possible nearby settlments using piece corners
          figure out the re

        *)
          failwith "not implemented"
        | MaritimeTrade (r_sold, r_bought) -> failwith "not implemented"
        | DomesticTrade (other_player, active_player_cost, other_player_cost) ->
          failwith "not implemented"
        | BuyBuild b ->
          begin
            match b with
            |BuildRoad rd -> failwith "not implemented"
            | BuildTown pt -> failwith "not implemented"
            | BuildCity pt -> failwith "not implemented"
            | BuildCard -> failwith "not implemented"
          end
        | PlayCard pc ->
          begin
            match pc with
            | PlayKnight r -> failwith "not implemented"
            | PlayRoadBuilding (rd, rd_o) -> failwith "not implemented"
            | PlayYearOfPlenty (r, r_o) -> failwith "not implemented"
            | PlayMonopoly r -> failwith "not implemented"
          end 
        | EndTurn ->
          let next_p = next_turn g.turn.active in
          let next_t = new_turn next_p in 
            { board = g.board;
              blue = g.blue;
              red = g.red;
              orange = g.orange;
              white = g.white;
              turn = next_t; 
              next = (next_p, ActionRequest)}
      end
  in (None, updated_game) (*CHANGE!*)
  (*Check win conditions. If someone wins return (Some winning_color, updated_game)
  otherwise, return (None, updated_game)*)

(* OLD--Use the new version above
let handle_move g m = 

  if validmove g m  then
    begin
    (*I'm guessing we'll want to keep track of the request that m corresponds to.
    Maybe that's in turn/next*)
    match m with
    | InitialMove l -> failwith "not implemented" 

      (*actually, never mind-- I ought to move this code to the body of validmove*)
      (*
      let valid_moves = valid_inital_moves g in
        if (List.mem) ((=) l) valid_moves then
          (*update game state*)
        else 
          let move = get_some (pick_random valid_moves) in
            (*update game state*)
        *)

    | RobberMove (p, c_opt) -> failwith "not implemented"
      (*Assuming p is a valid location, change 
      the board's robber location to p. Then remove a random resource from
      the player with color c_opt and give it to the active player*)
    | DiscardMove c -> failwith "not implemented"
  (*     I assume this involves subtracting c from the player
      who made the discard move. I'm not quite sure how you tell which player
      discarded, though *)
    | TradeResponse b -> failwith "not implemented"
      (*If true, then conduct the trade (and don't conduct
      the trade if false). Then return control to the active player.*)
    | Action a ->
      begin
        match a with
        | RollDice -> failwith "not implemented"
          (*Generate a random dice roll. If seven, handle discards, then ask the
          active player where they want to place the robber*)
        | MaritimeTrade (r_sold, r_bought) -> failwith "not implemented"
          (*check that the player can
          conduct this trade. If so, take away r_sold from the active 
          player and give them r_bought.*)
        | DomesticTrade (other_player, active_player_cost, other_player_cost) ->
          failwith "not implemented"
          (*Send a trade request to other player*)
        | BuyBuild b ->
          begin
            match b with
            |BuildRoad rd -> failwith "not implemented"
            | BuildTown pt -> failwith "not implemented"
            | BuildCity pt -> failwith "not implemented"
            | BuildCard -> failwith "not implemented"
          end
        | PlayCard pc ->
          begin
            match pc with
            | PlayKnight r -> failwith "not implemented"
            | PlayRoadBuilding (rd, rd_o) -> failwith "not implemented"
            | PlayYearOfPlenty (r, r_o) -> failwith "not implemented"
            | PlayMonopoly r -> failwith "not implemented"
          end 
        | EndTurn -> failwith "not implemented"
          (*Pass control to the next player*)
      end
    end
  else 
    begin
      match g.next with | None 
    end

*)




let presentation g = failwith "Were not too much to pay for birth."

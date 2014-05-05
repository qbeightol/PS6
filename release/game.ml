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
        | RobberMove x -> robber_helper g x
        | DiscardMove c -> discard_helper g c
        | TradeResponse b -> trade_helper g b
        | ActionRequest ->
          if is_none g.turn.dicerolled then Action(RollDice) 
                                       else Action(EndTurn)
  in
  let updated_game = 
    match move with
    | InitialMove l -> failwith "not implemented"
    | RobberMove x -> robber_helper g x
    | DiscardMove c -> discard_helper g c
    | TradeResponse b -> trade_helper g b
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
        | MaritimeTrade x -> maritime_helper g x
        | DomesticTrade x -> domestic_helper g x
        | BuyBuild b -> buyBuild_helper g b
        | PlayCard pc -> playCard_helper g pc
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
  in (None, updated_game) 


let presentation g = failwith "Were not too much to pay for birth."

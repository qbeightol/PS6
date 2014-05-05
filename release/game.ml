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

let handle_move g m = 
  (*I'm guessing we'll want to keep track of the request that m corresponds to.
  Maybe that's in turn/next*)
  match m with
  | InitialMove l -> failwith "not implemented" 
    (*Check for valid placement. If the placement is valid, 
    update the map. Otherwise, insert a minimum viable move?*)
  | RobberMove (p, c_opt) -> robber_helper g (p, c_opt)

    (*actually, never mind-- I ought to move this code to the body of validmove*)
    (*
    let valid_moves = valid_inital_moves g in
      if (List.mem) ((=) l) valid_moves then
        (*update game state*)
      else 
        let move = get_some (pick_random valid_moves) in
          (*update game state*)
      *)

    (*Assuming p is a valid location, change 
    the board's robber location to p. Then remove a random resource from
    the player with color c_opt and give it to the active player*)
  | DiscardMove c -> discard_helper g c
(*     I assume this involves subtracting c from the player
    who made the discard move. I'm not quite sure how you tell which player
    discarded, though *)
  | TradeResponse b -> trade_helper g b
    (*If true, then conduct the trade (and don't conduct
    the trade if false). Then return control to the active player.*)
  | Action a ->
    begin
      match a with
      | RollDice -> failwith "not implemented"
        (*Generate a random dice roll. If seven, handle discards, then ask the
        active player where they want to place the robber*)
      | MaritimeTrade x -> maritime_helper g x
        (*check that the player can
        conduct this trade. If so, take away r_sold from the active 
        player and give them r_bought.*)
      | DomesticTrade x -> domestic_helper g x
        (*Send a trade request to other player*)
      | BuyBuild b -> buyBuild_helper g b
      | PlayCard pc -> playCard_helper g pc
      | EndTurn -> failwith "not implemented"
        (*Pass control to the next player*)
    end
    

let presentation g = failwith "Were not too much to pay for birth."

open Definition
open Constant
open Util
open Print
open GameType
open GameHelper
open MoreUtil


type game = GameType.t

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
    } in
  let (blue, red, orange, white) = to_player_tuple plist in
  let (blue, red, orange, white) = ratio_helper (blue, red, orange, white) settlements ports in
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
        | RobberRequest -> failwith "not implemented" (*robber_helper g*)
        | DiscardRequest -> failwith "not implemented" (*discard_helper g*)
        | TradeRequest -> failwith "not implemented" (*trade_helper g*)
        | ActionRequest ->
          if is_none g.turn.dicerolled then Action(RollDice) 
                                       else Action(EndTurn)
  in
  let updated_game = 
    match move with
    | InitialMove l -> initial_helper g l
    | RobberMove x -> robber_helper g x
    | DiscardMove c -> discard_helper g c
    | TradeResponse b -> trade_helper g b
    | Action a ->
      begin
        match a with
        | RollDice -> 
          let roll = random_roll () in 
            if roll = cROBBER_ROLL then 
              match discard_player g with
              | None -> 
                {g with turn = {g.turn with dicerolled = Some roll};
                        next = (g.turn.active, RobberRequest)}
              | Some color -> 
                {g with turn = {g.turn with dicerolled = Some roll};
                        next = (color, DiscardRequest)}


              (*part of A2*)
              (*handle discards and moving the robber
                i.e.
                + issue discard requests to any player that has more than 
                  cMAX_HAND_SIZE cards
                + ask the active player to move the robber

                This is kind of tricky since you'll need to handle multiple 
                discard requests. You'll need to issue one discard request
                or robber request right away, and if they're are other requests
                that need to be served, you'll need to add logic to the 
                DiscardMove match statement that handle those requests.
                (But I'm pretty certain that 
                discard moves are only made when the robber is rolled. So 
                implementing that logic might be as simple as checking--within
                the DiscardMove match statement--whether any players have more
                than the maximum hand size. If someone does, issue them a 
                discard request. Otherwise, ask the active player to move the
                robber. NOTE: DON'T CHANGE THE ACTIVE PLAYER WHEN ISSUING
                DISCARD REQUESTS)
              *)
            else resource_gen g roll
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
  in 
  print_update g.turn.active m (state_of_game updated_game);
  is_winner g updated_game


let presentation g =
  let board = 
    { map = g.board.map;
      structures = g.board.structures;
      deck = hide g.board.deck;
      discard = g.board.discard;
      robber = g.board.robber
    }
  in
  let blue = present_player_info g.turn.active Blue g.blue in
  let red = present_player_info g.turn.active Red g.red in
  let orange = present_player_info g.turn.active Orange g.orange in
  let white = present_player_info g.turn.active White g.white in
  let turn = 
  { active = g.turn.active;
    dicerolled = g.turn.dicerolled;
    cardplayed = g.turn.cardplayed;
    (*although trades should happen before buys, I'll hide this just in case
    something weird happens*)
    cardsbought = 
      begin
        let (c, r) = g.next in 
          if g.turn.active = c then g.turn.cardsbought
          else  hide g.turn.cardsbought
      end;
    tradesmade = g.turn.tradesmade;
    pendingtrade = g.turn.pendingtrade
  }
  in
  { board = board;
    blue = blue;
    red = red;
    orange = orange;
    white = white;
    turn = turn; 
    next = g.next
  }

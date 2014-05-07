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
        | RobberRequest -> 
          RobberMove (get_some (pick_random (valid_robber_moves g)))
        | DiscardRequest -> 
          let (b, w, o, g, l) = inv g c in
          let mkd_inv = ((Brick, b), (Wool, w), (Ore, o), (Grain, g), (Lumber, l)) in
          let f acc (r, c) = (list_gen c r)::acc in
          let discard_list = List.flatten (fold_5tuple f [] mkd_inv) in
          let qty_to_discard = (b+w+o+g+l)/2 in
          let rec loop n d_list = 
            if n = 0 then [] 
            else 
              let (d, rem_d_list) = pick_one d_list in 
                d::(loop (n-1) rem_d_list)
          in
          let discards = loop qty_to_discard discard_list in
          let costs = List.map single_resource_cost discards in
          let tot_cost = List.fold_left (map_cost2 (+)) (0,0,0,0,0) costs in
            DiscardMove tot_cost
        | TradeRequest -> 
          TradeResponse (get_some (pick_random [true; false]))
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
  print_update g.turn.active move (state_of_game updated_game);
  (None, updated_game) 


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

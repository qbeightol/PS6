open Definition
open Constant
open Util
open Print
open GameType


(*Add a function that checks whether a move is valid*)

(*a player record with no resources, cards, knights, nor trophies*)
let empty_pr =
  let inventory : costrecord = 
    { bricks = 0;
      wool = 0;
      ore = 0;
      grain = 0;
      lumber = 0
    }
  in
  { inventory = inventory;
    cards = Reveal [];
    knights = 0;
    longestroad = false;
    largestarmy = false;
  }

type pr = playerrecord

let to_player_tuple (plist: player list) : (pr * pr * pr * pr) =
  match plist with
  | _::_::_::[_] ->
    let f (color, (inventory, cards), (k, lr, la)) (blu, red, org, wht) =
      let (b, w, o, g, l) = inventory in 
      let new_inventory = 
        { bricks = b;
          wool = w;
          ore = o;
          grain = g;
          lumber = l
        }
      in 
      let new_record = 
        { inventory = new_inventory;
          cards = cards;
          knights = k;
          longestroad = lr;
          largestarmy = la
        }
      in
      match color with
      | Blue -> (new_record, red, org, wht)
      | Red ->  (blu, new_record, org, wht)
      | Orange -> (blu, red, new_record, wht)
      | White -> (blu, red, org, new_record)
    in 
    List.fold_right f plist (empty_pr, empty_pr, empty_pr, empty_pr) 
  | _ -> failwith "invalid player list"
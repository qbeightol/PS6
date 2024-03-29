open Definition
open Registry
open Constant
open Util
open MoreUtil

(** Give your bot a 2-20 character name. *)
let name = "myopicbot"

(*Planning

  Initial Requests
  * I think the simplest thing to do is picking an intersection that will 
    produce the most "useful" resources
    + variety is probably good
    + common rolls are also goood
    + proximity to ports is good
  * There's some potential to interefere with your opponent's ability to expand.
    + You could position yourself to be closer (than your opponents) to 
      potentially valuable resources. 
    + You could expand to those regions first
    + Or you could use roads to limit your opponents expansion
    + But it might be hard to implement

  Robber Requests
  * If the bot rolls a seven, take resources from the player who's allocation makes
    it the most likely for the bot to steal a useful resource.
  * I guess I'll have to define what resources are useful. 
  * Alternately, I might consider what would happen when I stole from each
    player, and compare how the bot does in each case
  * Finally, I'll want to consider which resources I want to deprive other players
    of. 

  Discard Request
  * This probably will behave like robber requests--the main difference will
    be a focus on giving up the least valuable goods. 
  * Again, this computation might be a little convluted, since some goods might
    offer significant bargaining power--i.e. they're hard for other players to 
    acquire w/o making expensive maritime trades. It might be wortwhile to
    figure out how much opponents value your resources.

  TradeRequest
  * The state evaluation function should have some way of evaluating resources;
    I should be able to compare the two resources and see if the trade is
    advantageous--i.e. the value of the resources the bot recevies is greater
    than the value of the resources it looses.
  * Hmm--I kind of worried that trades might not happen. When I write the
    state/resource evaluation function, I might want to consider the 
    opportunity costs of goods. That'll be a little tricky to calculate, but
    it might encourage trading (or at least prompt the bot to accept 
    advantageous trades it might not have accepted before)
    + Actually, it might be easiest to compute opportunity costs using maritime 
      trade. The oc of a good would be 4/3/2 resources of any kind. Of course,
      this method ignores the possibility of building a new settlement and then
      collecting the desired resource through that settlement. But it seems like
      maritime trading will almost always be the most effecient way to get one
      of any resource.

  Action Request
  * Roll the dice immediately
  * Generate potential plays for an entire turn
  * Evaluate the resulting states
  * Do the move that leaves you the bot in the best shape at the end of its 
    turn

*)


module Bot = functor (S : Soul) -> struct
  let cVP_ROAD = 0.3 (*A road's approximate worth in victory points--this 
    is kind of handwavy. You'd certainly could get 2 victory points simply by
    owning five roads, but you'd probably need to much more--maybe 10 to 
    actually have a firm grasp on the trophy (hence a value greater than .2) 
    Adding another tenth a point seems reasonable, given that roads do have 
    some strategic value, and in general, its probably good to encourage the 
    bot to build roads.*)
  let cVP_KNIGHT = 0.3
  let cVP_ROAD_BUILDING = cVP_ROAD *. 2. -. 0.1
  (*let cVP_YEAR_OF_PLENTY = *)

  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  type values = (float * float * float * float * float)

  (*looks at builds to determine the value of resources*)
  let analyze_build ((c: cost), (v: float)) : values = 
    let total_cost = sum_cost c in
    let f r = (float r) /. float(total_cost) *. v in
      map_cost f c

  (*Determines the expected value of a card drawn from the deck*)
  let calc_card_value (s: state) : float = 
    let ((_, _, deck, discard, _), players, _, (c, _)) = s in
    let is_active_player (color, _, _) = (color = c) in
    let (_, (_, cards), _) = List.find is_active_player players in
      let card_list = reveal cards in
      let dealt_vps = list_count ((=) VictoryPoint) (card_list@discard) in
      let remaining_vps = cNUM_VICTORYPOINT - dealt_vps in
      (*Assumes that the game is played with the default deck*)
      let total_cards = List.length (reveal cDEFAULT_DECK) in
      let undrawable_cards = List.length (card_list) + List.length (discard) in
      let drawable_cards = total_cards -undrawable_cards in
        ((float remaining_vps) /. float(drawable_cards)) *. float (cVP_CARD)

  (*returns a record containing rough estimates of how much the active player 
  values each resource*)
  (*update to evaluate from a particular color's perspective?*)
  let eval_resources (s: state) (c: color) =
    let card_value = calc_card_value s in
    let builds = [(cCOST_ROAD, cVP_ROAD); (cCOST_TOWN, float cVP_TOWN); 
          (cCOST_CITY, float (cVP_CITY-cVP_TOWN)); (cCOST_CARD, card_value)]
    in 
    let values = List.map analyze_build builds in
    let f (b, w, o, g, l) (b_a, w_a, o_a, g_a, l_a) =
      (b +. b_a, w +. w_a, o +. o_a, g +. g_a, l +. l_a)
    in
    List.fold_left f (0., 0., 0., 0., 0.) values

  let calc_yop_value s c =
    let (b_val, w_val, o_val, g_val, l_val) = eval_resources s c in
    2. *. (max b_val (max w_val (max o_val (max g_val l_val))))

  let calc_monop_value s c = failwith "not implemented"

  let calc_hand_value s c cs =
    let value = function
      | Knight -> cVP_KNIGHT
      | VictoryPoint -> float cVP_CARD
      | RoadBuilding -> cVP_ROAD_BUILDING
      | YearOfPlenty -> calc_yop_value s c
      | Monopoly -> calc_monop_value s c
    in
    let f acc e = acc +. (value e) in
      List.fold_left f 0. cs

  let calc_approx_sett_vp = function
      | None -> (0., 0., 0., 0.) 
      | Some (Blue, sett) -> (float (settlement_num_vp sett), 0., 0., 0.)
      | Some (Red, sett) -> (0., float (settlement_num_vp sett), 0., 0.)
      | Some (Orange, sett) -> (0., 0., float (settlement_num_vp sett), 0.)
      | Some (White, sett) -> (0., 0., 0., float (settlement_num_vp sett))

  let calc_approx_vp s : (float * float * float * float) =
    let ((_, (setts,_), _, _, _), players, _,_) = s in
    let sett_vps = 
      let f (vpb, vpr, vpo, vpw) e = 
        let (e_vpb, e_vpr, e_vpo, e_vpw) = calc_approx_sett_vp e in
          (vpb +. e_vpb, vpr +. e_vpr, vpo +. e_vpo, vpw +. e_vpw)
      in
      List.fold_left f (0., 0., 0., 0.) setts
    in
    let card_value = calc_card_value s in
    let player_vps = 
      let player_vp (color,(_, cards),(_, lr, la)) = 
        let card_vp =
          match cards with
          | Hidden n -> (float n) *. card_value
          | Reveal cs -> calc_hand_value s color cs
        in
        let lr_vp = if lr then float cVP_LONGEST_ROAD else 0. in
        let la_vp = if la then float cVP_LARGEST_ARMY else 0. in
        let tot_vp = card_vp +. lr_vp +. la_vp in
          match color with
          | Blue -> (tot_vp, 0., 0., 0.)
          | Orange -> (0., tot_vp, 0., 0.)
          | Red -> (0., 0., tot_vp, 0.)
          | White -> (0., 0., 0., tot_vp)
      in
      let f acc e = map_4tuple2 (+.) acc (player_vp e) in
        List.fold_left f (0.,0.,0.,0.) players
    in
    map_4tuple2 (+.) sett_vps player_vps

      




  (*evaluates the state's "goodness" from each player's perspective; returns
  the tuple (b,r,o,w) where each entry represents the goodness of the state
  in terms of victory points*)
  let eval_state (s : state) (c: color) : (float * float * float * float) =
    (*
    let ((map, (setts, roads), _, _, robber), players, _, _) = s in
    (*settlement points*)
    let (spb, spr, spo, spw) = ... in
    (*road points*)
    let (rpb, rpr, rpo, rpw) = ... in
    (*player points*)
    *)
    failwith "not implemented"

  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    match r with
    | InitialRequest -> failwith "not implemented"
      (*use the helper funtions in MoreUtil to create a list of valid moves.
      Evaluate the resulting states, and choose a random best move*)
    | RobberRequest -> failwith "not implemented"
      (*consider putting the robber on each tile, and stealing resources from
      each possible player. Then choose a random move out of one of the "best
      moves"*)
    | DiscardRequest->
      failwith "not implemented"
      (*evaluate your resources, and discard the least valuable ones.
      I may want to make some exceptions to ensure that the bot retains
      a reasonably diverse hand.*)
    | TradeRequest -> failwith "not implemented"
      (*compare states before and after the request. If it increases
      the difference between your score and the next highest player's
      score, accept it.*)
    | ActionRequest -> 
      if is_none t.dicerolled then Action(RollDice) 
      else failwith "not implemented"
      (*I'll first need to ponder some trades.
        Here, I think the best solution may be to*)
      (*Then I'll need to focus on what makes sense to buy*)

    
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
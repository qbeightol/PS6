open Definition
open Registry
open Constant
open Util

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
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  type values = (float * float * float * float * float)

  (*looks at builds to determine the value of resources*)
  let analyze_build (c: cost), (v: float)) : values = 
    let total_cost = sum_cost c in
    let f r = (float (r / total_cost)) *. v in
      map_cost f c

  (*Determines the expected value of a card drawn from the deck*)
  let calc_card_value (s: state) : float = 
    let ((_, _, deck, discard, _), players, _, (c, _)) = s in
    let is_active_player (color, _, _) = (color = c) in
    let (_, (_, cards), _) = listmem_remove is_active_player players in
      let card_list = reveal cards in
      let dealt_vps = list_count ((=) VictoryPoint) (card_list@discard) in
      let remaining_vps = cNUM_VICTORYPOINT - dealt_vps in
      (*Assumes that the game is played with the default deck*)
      let total_cards = List.length (reveal cDEFAULT_DECK) in
      let undrawable_cards = List.length (card_list) + List.length (discard) in
      let drawable_cards = total_card -undrawable_cards in
        ((float remaining_vps) /. float(drawable_cards)) *. cVP_CARD


  type resourcevalues = {
    b = float;
    w = float;
    o = float;
    g = float;
    l = float
  }


  (*returns a record containing rough estimates of how much the active player 
  values each resource*)
  let eval_resources (s: state) : resourcevalues =
  (* Cost to build the four build types
  let cCOST_ROAD : cost = (1,0,0,0,1)
  let cCOST_TOWN : cost = (1,1,0,1,1)
  let cCOST_CITY : cost = (0,0,3,2,0)
  let cCOST_CARD : cost = (0,1,1,1,0)
  
  let cVP_CARD = 1 (* per victory card *)
  let cVP_TOWN = 1 (* per town *)
  let cVP_CITY = 2 (* per city *)

  *)


    let cVP_ROAD = 0.3 (*A road's approximate worth in victory points--this 
    is kind of handwavy. You'd certainly could get 2 victory points simply by
    owning five roads, but you'd probably need to much more--maybe 10 to 
    actually have a firm grasp on the trophy. Adding another tenth a point
    seems reasonable, given that roads do have some strategic value, and in
    general, its probably good to encourage the bot to build roads, since 
    they'll help the bot build settlements.*)
    in
    let card_value = calc_card_value s in
    let builds = [(cCOST_ROAD, cVP_ROAD); (cCOST_TOWN, cVP_TOWN); 
          (cCOST_CITY, cVP_CITY-cVP_TOWN); (cCOST_CARD, card_value)]
    in 
    let values = List.map analyze_build builds
    let f (b, w, o, g, l) (b_a, w_a, o_a, g_a, l_a) =
      (b +. b_a, w +. w_a, o +. o_a, g +. g_a, l +. l_a)
    in
    List.fold_left f (0., 0., 0., 0., 0.) values


  (*I'm still deciding on what, exactly, this function will do. In general, 
    its supposed to return a value that represents the "goodness" of the 
    state (most likely from the player's point of view). 

    It's a little tricky to implement, however, since I'm not sure how the a.i.
    will predict the behavior of other bots, since their behavior isn't dictated
    by the "goodness" from the perspective of the current player. They should
    consider the "goodness" of all players, and make decisions that mazimize
    their goodness relative to the other players. 

    It might be reasonable to return a tuple of ints, which represents the 
    goodness of the state from each players pespective. Alternately,
    I might return a tuple of floats. 

    Or I might return a float indicating the "goodness" of the state from
    just one players perspective. 

    Record of floats?

    yeah, floats seem like a good idea. That way I can have the numbers
    approximately represent the number of victory points each player has
  *)

  let eval_state (s : state) : (float * float * float * float) =
    failwith "not implemented"

  let eval_resources 

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
      (*I'll first need to ponder some trades*)
      (*Then I'll need to focus on what makes sense to buy*)

    
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
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

    yeah, floats seem like a good idea. That way I can have the numbers
    approximately represent the number of victory points each player has*)
  let eval_state (g:game) : (int * int * int * int) =
    failwith "not implemented"


  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    match r with
      | InitialRequest -> failwith "not implemented"
      | RobberRequest -> failwith "not implemented"
      | DiscardRequest-> failwith "not implemented"
      | TradeRequest -> failwith "not implemented"
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) 
        else failwith "not implemented"

    
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
open Definition
open Registry
open Constant
open Util

(** Give your bot a 2-20 character name. *)
let name = "myopicbot"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else Action(EndTurn)

    (*I'm still deciding on what, exactly, this function will do. In general, 
      its supposed to return a value that represents the "goodness" of the 
    state (most likely from the player's point of view). 

    It's a little tricky to implement, however, because I'm not sure how the a.i.
    will predict the behavior of other bots, since their behavior isn't dictated
    by the "goodness" from the perspective of the current player. They should
    consider the "goodness" of all players, and make decisions that mazimize
    their goodness relative to the other players. 

    It might be reasonable to return a tuple of ints, which represents the 
    goodness of the state from each players pespective. Alternately,
    I might return a tuple of floats. 

    yeah, floats seem like a good idea. That way I can have the numbers
    approximately represent the number of victory points each player has*)
    let eval_state 
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
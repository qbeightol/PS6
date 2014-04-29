open Definition
open Constant
open Util
open Print


type game = state (*Is anything else worth including/calculating?*)

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())
    (*Change to game_of_state(gen_random_initial_state ())*)


let handle_move g m = failwith "If all the soul-and-body scars"


let presentation g = failwith "Were not too much to pay for birth."

open Definition
open Constant
open Util
open Print


type game = state (*Is anything else worth including/calculating?*)

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())
    (*Change to game_of_state(gen_random_initial_state ())*)


let handle_move g m = 
    (*I'm guessing we'll wnat to keep track of the request that m corresponds to.
    Maybe that's in turn/next*)
    match m with
    | InitialMove l ->
    | RobberMove r ->
    | DiscardMove c -> (*I assume this involves subtracting c from the player
        who made the discard move. I'm not quite sure how you tell which player
        discarded, though*)
    | TradeResponse b -> (*If true, then conduct the trade (and don't conduct
        the trade if false). Then return control to the active player.*)
    | Action a ->
        begin
            match a with
            | MaritimeTrade mt -> 
                match mt with (r_sold, r_bought) ->
            | DomesticTrade t ->
                match t with (other_player, active_player_cost, other_player_cost) ->
            | BuyBuild b ->
                begin
                    match b with
                    |BuildRoad rd -> 
                    | BuildTown pt -> 
                    | BuildCity pt ->
                    | BuildCard ->
                end
            | Playcard pc -> 
                begin
                    match pc with
                    | PlayKnight r ->
                    | PlayRoadBuilding (rd, rd_o) ->
                    | PlayYearOfPlenty (r, r_o) -> 
                    | PlayMonopoly r -> 
                end 
            | EndTurn -> (*Pass control to the next player*)
        end


let presentation g = failwith "Were not too much to pay for birth."

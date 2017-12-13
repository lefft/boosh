# devtools::install_github("michael-franke/game_theoretic_pragmatics_ORE", subdir="gtp")
# 1. call, for example, `appply_RD(M_implicature_game)` 
#    for an application of the replicator dynamic (discrete time, behavioral
#    strategies) to the M-implicature context model
# 2. use function `create_game()` to create more examples

library("gtp")

gtp::free_choice_game
gtp::scalar_implicature_game
gtp::apply_RSA

function (game, depth = 10, lambda = 5){
  rec = prop.table(game$semantics, 1)
  sen = NA
  if (depth >= 1) {
    for (i in 1:depth) {
      sen = quantal_response(get_EU_sender_RSA(rec, game), 
                             lambda)
      rec = get_receiver_belief(sen, game)
    }
  }
  return(list(sen = sen, rec = rec))
}
gtp::get_EU_sender_RSA
gtp::quantal_response
gtp::get_receiver_belief




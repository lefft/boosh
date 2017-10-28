# load dependencies
lefftpack::quiet_attach(
  "magrittr","dplyr","reshape2","ggplot2","grid","gridExtra"
)
theme_set(theme_minimal(13)); save_plots <- TRUE

# load the modeling functions
source("schellpo_functions.r")

# number of agents per group
type_set_size <- 100

# the agents, as a list
Tlist <- list(
  a = paste0("a", sprintf("%02d", 1:type_set_size)), 
  b = paste0("b", sprintf("%02d", 1:type_set_size))
)

# payoff id
payoff_id <- "both-a-only"

# the utility/payoff function
segU <- function(bigT, type1, type2){
  stopifnot(type1 %in% names(bigT), type2 %in% names(bigT))
  if (type1=="a" | type2=="a"){
    return(10)
  } else {
    return(0)
  }
}

# run the simulation given data in `Tlist` and utility defined by `segU()`
survival_sims <- simulate_generations(Tlist, segU, n_gens=1e3, "all")

cap_grob <- textGrob(paste(
  "parameters: ", 
  "randomly sampling pairs of agents of type 'a' or 'b'.",
  "payoff is 10 for an (a, a)-pair and 0 otherwise.",
  "positive payoff results in survival; non-positive results in death.",
  sep="\n  > "
), just="left")

# simulate some generations and plot num left over time
survive_n_plot <- 
  survival_sims$res_long %>% 
  ggplot(aes(x=generation, y=left, color=type)) + 
  geom_line() + 
  scale_color_manual(values=c("forestgreen","orange")) + 
  scale_y_continuous(limits=c(0, type_set_size), 
                     breaks=seq(from=0, to=type_set_size, length.out=5)) + 
  labs(color="agent type", x="", y="number of \nagents remaining") + 
  theme(legend.position="bottom")

# simulate some generations and plot prop cat 'a' left over time
survive_rate_plot <- 
  survival_sims$res_wide %>% melt(measure.vars=c("prop_a","prop_b")) %>% 
  ggplot(aes(x=generation, y=value, color=variable)) + 
  geom_line() + 
  scale_color_manual(labels=c("a","b"), values=c("forestgreen","orange")) + 
  scale_y_continuous(
    limits=c(0,1), breaks=seq(from=0, to=1, length.out=5),
    labels=gsub("0\\.", "\\.", seq(from=0, to=1, length.out=5))
  ) + theme(legend.position="none") + 
  labs(color="agent type", y="proportion of\nremaining agents") 
  
# show them both together
(survive_plot <- grid.arrange(arrangeGrob(
  grobs=list(cap_grob, survive_n_plot, survive_rate_plot),
  heights=c(2, 6, 6)
)))


if (save_plots){ggsave(
  plot=survive_plot, filename=paste0(
    "plot_survival_", payoff_id, "_", type_set_size, "n_groups.pdf"
  ), height=6, width=10, units="in"
)}


### appendix: REPLICATION OF POTTS 2008 MODELS --------------------------------
if (FALSE){
# a symmetric evolutionary game is a struct (segT, segU), where:
# 
# type set component is [defn 1.i]
segT <- c("a", "b")
# but this is a better representation bc it has actual sets, not just names:
type_set_size <- 100
Tlist <- list(
  a = paste0("a", sprintf("%02d", 1:type_set_size)), 
  b = paste0("b", sprintf("%02d", 1:type_set_size))
)
# 
# utility component is [defn 1.ii]
#     A   B     [item 14]
# A  10  10     payoff matrix 
# B  10   0     implemented in `segU()`
segU <- function(bigT, type1, type2){
	stopifnot(type1 %in% names(bigT), type2 %in% names(bigT))
	if (type1=="a" | type2=="a"){
		return(10)
	} else {
		return(0)
	}
}

# these facts are derived -- see below
#   "if we begin with pct_i("A") == .5, then we obtain the following
#    initial measures of fitness:" (p11)
# 	  - fitness_type(T, util, "A") == 10
# 	  - fitness_type(T, util, "B") == 5
# 	  - fitness_pop(T, util) == 7.5
# 	  - pct_i_update(T, util, "A") == .6666
# 
# item 15 -- avg fitness of type 'a'       a-fit     ~~~> 10 
fitness_type(Tlist, util=segU, type_i="a")
# item 16 -- avg fitness of type 'b'       b-fit     ~~~> 5
fitness_type(Tlist, util=segU, type_i="b")
# item 17 -- avg fitness of pop            pop-fit   ~~~> 7.5
fitness_pop(Tlist, util=segU)
# item 18 -- pct 'a' after one generation  t1(pct-a) ~~~> .666
pct_i_update(Tlist, util=segU, type_i="a")   

# simulate a single round of the game, for a single pair of players
run_game_round(Tlist, segU)

# simulate some generations and plot num left over time
simulate_generations(Tlist, segU, n_gens=100, "data_long") %>% 
  ggplot(aes(x=generation, y=left, color=type)) + geom_line() + 
  scale_y_continuous(limits=c(0, type_set_size))

# simulate some generations and plot prop cat 'a' left over time
simulate_generations(Tlist, segU, n_gens=100, "data_wide") %>% 
  ggplot(aes(x=generation, y=prop_a)) + 
    geom_line() + scale_y_continuous(limits=c(0,1))
}



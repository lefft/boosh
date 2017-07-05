library("dplyr"); library("magrittr"); library("reshape2"); library("ggplot2")
source("topheavy_sims_functions_globals.r")

### topheavy sim ##############
# === === === === === === 
team_weights <- team_wts()


num_sims <- 2e2
winnaz <- data.frame(
  team  = sapply(seq_len(num_sims), function(x) sim_nba()$finals$winner),
  simnum = seq_len(num_sims), 
  stringsAsFactors=FALSE
)

simsum <- winnaz %>% group_by(team) %>% 
  summarize(num_chips = n()) %>% left_join(confs, by="team") %>% 
  left_join(team_weights, by="team") %>% 
  mutate(team = gsub("_east|_west", "", team)) %>% 
  arrange(num_chips) %>% 
  mutate(team = factor(team, levels=team))#levels=c(east,west)))

ggplot(simsum, aes(x=team, y=num_chips, fill=weight)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("red","green","yellow")) + 
  # facet_wrap(~conf) + 
  # scale_fill_manual(values=c("#4b7cbc", "#bc6262")) + 
  coord_flip() +
  theme(legend.position="top")
ggsave(filename="weighted_sim20k_results.pdf", height=8, width=6, units="in")





### SCRATCH AREA ##############################################################
# === === === === === === === === === === === === === === === === === === 

if (FALSE){
  theme_set(theme_minimal(11))
  ggplot(dat, aes(x=wins)) + 
    geom_density() +
    # geom_histogram(bins=25) +
    facet_wrap(~year, ncol=5, scales="free_x") + 
    scale_x_continuous(limits=c(0,82), breaks=seq(0, 75, 15)) + 
    theme(panel.grid.minor.y=element_blank(), axis.text.y=element_blank()) 
  ggsave(filename="wintotals_hist.pdf", width=8.5, height=11, units="in")
}



n_teams <- 30
num_sims <- 100
sim_results <- data.frame(matrix(rep(NA, n_teams*num_sims), ncol=30))
names(sim_results) <- team_names

for (x in seq_len(num_sims)){
  sim_results[x, ] <- simulate_season(30, 82, wins_sd)
}

sapply(sim_results, max)

# c("ATL_east","BOS_east","BRK_east","CHI_east","CHO_east","CLE_east","DET_east","IND_east","MIA_east","MIL_east","NYK_east","ORL_east","PHI_east","TOR_east","WAS_east","DAL_west","DEN_west","GSW_west","HOU_west","LAC_west","LAL_west","MEM_west","MIN_west","NOP_west","OKC_west","PHO_west","POR_west","SAC_west","SAS_west","UTA_west")

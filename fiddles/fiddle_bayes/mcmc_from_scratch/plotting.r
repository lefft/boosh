# load dependencies + get stuff set up
library("dplyr"); library("magrittr"); library("ggplot2")
source("mcmc_funcs.r")
theme_set(theme_minimal())



mh_mcmc_(numit=1e4) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin0.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.10) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin10.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin25.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.50) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin50.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.75) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin75.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.90) %>% plot_mcmc_() %>% ggsave(
  filename="plt_burn/mcmcrun_100k_burnin90.pdf", height=6, width=8, units="in"
)


# make plots for 
mh_mcmc_(numit=1e1, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_iter/mcmcrun_1e1.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e2, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_iter/mcmcrun_1e2.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e3, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_iter/mcmcrun_1e3.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e4, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_iter/mcmcrun_1e4.pdf", height=6, width=8, units="in"
)
mh_mcmc_(numit=1e5, burnin_prop=.25) %>% plot_mcmc_() %>% ggsave(
  filename="plt_iter/mcmcrun_1e5.pdf", height=6, width=8, units="in"
)

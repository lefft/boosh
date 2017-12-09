########## mnb from scratch ---------------------------------------------------
# [MAIN FUNC AT BOTTOM!] 
# TODO: 
#   - generalize to arbitrary classes 
#   - option for tf-idf weighting 
#   - option for freq weighting (instead of y/n as it it now) 
#   - shd be using coll(x) or fixed(x) in str_count()??? 
#   - need better setup for out of sample prediction 
#   - joyplot colored by tf-idf or no (refer to catalan ex)
lefftpack::lazy_setup(sparse_grid=TRUE)
library("ggridges")
source("spammie-spam-fonxe.r")

# uci spam data -- see also readme in same folder "uci_ml_spam-readme.txt" 
spam <- "http://lefft.xyz/stuff/datasets/uci_ml_spam.txt"

dat  <- read.csv(spam, sep="\t", quote="", stringsAsFactors=FALSE, 
                 header=FALSE, col.names=c("label", "text"))

# toss all punct to make life easier, stringr:: breaks otherwise (fix later)
dat$text <- gsub("[[:punct:]]", "", dat$text)

# monte carlo experiments to evaluate accuracy on this problem at a given n 
mnb_run <- function(n, replace=FALSE){
  idx <- sample(1:nrow(dat), size=n, replace=replace)
  spam_labs <- dat$label[idx]
  spam_preds <- naive_bayes_pred(docs=dat$text[idx], labels=dat$label[idx])
  message(round(sum(spam_labs == spam_preds)/n*100, 1), "% accuracy -- ", 
          sum(spam_labs == spam_preds), " correct preds, out of ", n) 
  return(sum(spam_labs == spam_preds)/n)
}

# n's to estimate accuracy for 
sizes <- seq(from=100, to=1000, by=100)
# sizes <- seq(from=100, to=2500, by=400)
# sizes <- seq(from=10, to=100, by=10); iters_per_size <- 10

# number of sample iterations at each n 
iters_per_size <- 100

# sample with replacement?  [NEXT DO W AND W/O REPL, THEN COLOR-CODE BY REPL] 
with_replacement <- FALSE


run_times <- setNames(rep(NA, times=length(sizes)), as.character(sizes))

# run iters_per_size-many experiments at each n in sizes, w or w/o replacement 
runs <- sapply(sizes, function(n){
  pre_time <- Sys.time()
  out <- replicate(iters_per_size, mnb_run(n, replace=with_replacement))
  post_time <- Sys.time()
  run_times[as.character(n)] <<- post_time - pre_time
  print(post_time - pre_time)
  return(out)
}) %>% set_colnames(paste0("n_", sizes)) %>% data.frame()

# visualize the sim results 
acc_dens_facet <- runs %>% melt(id.vars=NULL) %>% 
  ggplot(aes(x=value)) + geom_density(alpha=.85, color="gray") + 
  facet_wrap(~variable) +
  geom_vline(aes(xintercept=.9), linetype="dashed", color="gray")

acc_dens_joy <- runs %>% melt(id.vars=NULL) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(y=variable)) + 
  geom_density_ridges(aes(x=value), alpha=.75, from=.7, to=1) +
  # ggjoy::geom_joy(aes(x=value), alpha=.75) + #, from=.8, to=1)
  labs(x="accuracy", y="", 
       title="MNB spam/ham classification accuracy on UCI SMS dataset", 
       subtitle=paste0("curves approximated by ", 
                       iters_per_size, " random samples at each size n"))

acc_mean_by_n <- runs %>% melt(id.vars=NULL) %>% 
  filter(!is.na(value)) %>% 
  mutate(variable = as.numeric(gsub("n_", "", variable))) %>% 
  group_by(variable) %>% summarize(
    mean=mean(value), 
    sem=boot_se(value, dig=10), 
    sd=sd(value)
  ) %>% ungroup() %>% 
  mutate(sem = case_when(sem > 1 ~ 1, sem < 0 ~ 0, TRUE ~ sem)) %>% 
  ggplot(aes(x=variable, y=mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=0) + 
  scale_y_continuous(limits=c(.8, 1)) + 
  labs(x="sample size (n)", y="mean accuracy", 
       title="mean accuracy of MNB spam/ham classifier", 
       subtitle=paste0("across ", iters_per_size, " samples at each size n"))

sim_time_plot <- qplot(sizes, run_times) + 
  labs(x="sample size (n)", y="duration (min)", 
       title="time to estimate classification accuracy against size of sample", 
       subtitle=paste0("", iters_per_size, " MNB fits per value of n"), 
       caption=paste0("total time elapsed: ", # FIX TIME UNITS HERE 
                      round(sum(as.numeric(run_times)), 1)))

print(acc_dens_facet)
print(acc_dens_joy)
print(acc_mean_by_n)
print(sim_time_plot)


write.csv(runs, paste0("out/mnb-runs-01", ".csv"), row.names=FALSE)
write.csv(data.frame(n=names(run_times), total_time=run_times),
          "out/mnb-run-times.csv", row.names=FALSE)


ggsave(plot=acc_dens_facet, 
       filename="out/mnb-acc-dens.pdf", width=8, height=6, units="in")
ggsave(plot=acc_dens_joy, 
       filename="out/mnb-acc-dens-joi.pdf", width=8, height=6, units="in")
ggsave(plot=acc_mean_by_n, 
       filename="out/mnb-acc-mean.pdf", width=6, height=4, units="in")
ggsave(plot=sim_time_plot, 
       filename="out/mnb-sim-time.pdf", width=6, height=4, units="in")


# ring a alarm to lemme know is donezo time :p 
library("audio")
play(c(sin(1:50000/100)^(log(10000))^2,
       sin(1:50000/100)^(log(10000)),
       sin(1:50000/100)^(log(10000))^2,
       sin(1:50000/100)^(log(10000))))




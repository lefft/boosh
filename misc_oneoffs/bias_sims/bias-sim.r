#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
### TODO next: 
###   - reread paype (see link + excerpt at bottom)
###   - map out what to do before refactoring etc. 
###   - make sure both plots come from same samples 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

lefftpack::lazy_setup()
# REPRO THIS SIM: http://www.ruf.rice.edu/~lane/papers/male_female.pdf

print.pyramid <- function(pyr){
  pyrsumm <- pyr %>% group_by(level, mf, status) %>% count 
  mean_pos_mf <- round(tapply(pyr$level, pyr$mf, mean), 2)
  mean_score_mf <- round(tapply(pyr[['score']], pyr$mf, mean), 2)
  anno <- paste0(
    "mean pos: ", mean_pos_mf['male'], " (male), ",
    mean_pos_mf['female'], " (female)",
    "\nmean score: ", mean_score_mf['male'], " (male), ",
    mean_score_mf['female'], " (female)",
    "\nprop male: ", round(sum(pyr$mf=="male") / nrow(pyr), 3),
    ", prop new ppl: ", round(sum(pyr$status=="noonoo") / nrow(pyr), 3))
  print(pyr %>% group_by(level) %>% summarize(
    male = sum(mf=="male") / n(),
    female = sum(mf=="female") / n()
  ) %>% melt(id.vars="level") %>%
    ggplot(aes(x=level, alpha=level, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="", caption=anno) +
    scale_x_continuous(breaks=1:8) +
    scale_fill_manual(values=c("#2F742C", "#c8ab94")) +
    scale_alpha(guide=FALSE, range=c(.35,.85)) +
    scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, length.out=5)))
  # 
  # print(pyrsumm %>% group_by(level, mf) %>% summarize(n=sum(n)) %>%
  #         ggplot(aes(x=level, y=n, fill=mf)) +
  #         geom_bar(stat="identity", position="dodge") +
  #         labs(x="", y="", caption=anno))
  
  print(as_data_frame(unclass(pyr)), n=5) # UNCLASS FIRST BC RECURSION! 
  message(anno)
}

levels <- c(8,7,6,5,4,3,2,1)
level_sizes <- c(10,40,75,100,150,200,350,500)
lev_lkup <- setNames(level_sizes, levels)

n_positions <- sum(level_sizes)

incumbents <- data_frame(
  id = paste0("inc", sprintf("%04d", 1:n_positions)),
  mf = c(rep(c("male","female"), times=n_positions/2),"male"),
  level = as.integer(unlist(mapply(
    rep, x=levels, each=level_sizes, SIMPLIFY=TRUE))), 
  status = "oldie",
  score = rnorm(n_positions, mean=50, sd=10)
)

#n=3;x=2
noonoo <- function(n){
  out <- data_frame()
  for (x in 1:n){
    if (!exists("noonoo_id_counter")){
      noonoo_id_counter <- 1426 
    } else {
      noonoo_id_counter <<- noonoo_id_counter + 1 
    }
    noonoo_mf <- sample(c("male","female"), 1)
    score_dist_mean <- ifelse(noonoo_mf=="male", 50, 25)
    
    out <- rbind(out, data_frame(
      id = paste0("noo", sprintf("%04d", noonoo_id_counter)), 
      mf = noonoo_mf, 
      level = 1, status = "noonoo", 
      score=max(0, rnorm(1, mean=score_dist_mean, sd=10))))
  }
  return(out)
}


run <- function(pyr){
  # remove 15% of incumbents 
  p <- pyr[sample(1:nrow(pyr), size=.85*nrow(pyr)), ]
  
  # get number of positions at each level 
  lev_status <- p %>% group_by(level) %>% count %$% 
    setNames(n, level)
  
  # get number of open level-1 positions 
  open_1 <- lev_lkup['1'] - lev_status['1']
  
  # get number of open level-2+ positions 
  open_n <- sapply(2:8, function(n){
    lev_lkup[as.character(n)] - lev_status[as.character(n)]
  })
  
  # generate random new ppl w their scores 
  noo <- noonoo((sum(open_n) + open_1)*2)
  
  # repeat the following until all positions are filled: 
  
  # fill open level-1 positions, sampling from noos weighted by score 
  p <- p %>% 
    rbind(noo[sample(1:nrow(noo), size=open_1, prob=noo$score), ])
  
  # fill the n_m open positions for each level 2+:
  for (l in 2:8){
    l_minus1 <- p[p$level == (l-1), ]
    # sample from ppl at l-1, weighting by score 
    promoted <- l_minus1[
      sample(1:nrow(l_minus1), size=open_n[ac(l)], prob=l_minus1$score),
    ]
    promoted$level <- promoted$level + 1
    promoted_ids <- promoted$id
    
    p <- p %>% filter(!id %in% promoted_ids) %>% rbind(promoted)
  }
  class(p) <- c("pyramid", class(p))
  return(p)
}



pyramid <- incumbents 
class(pyramid) <- c("pyramid", class(pyramid)); print(pyramid)

p <- pyramid; print(p)
ggsave("00_p.pdf", width=4, height=3, units="in", scale=1.25)
newp <- run(p); print(newp)
ggsave("01_newp.pdf", width=4, height=3, units="in", scale=1.25)
newnewp <- run(newp); print(newnewp)
ggsave("02_newnewp.pdf", width=4, height=3, units="in", scale=1.25)
newnewnewp <- run(newnewp); print(newnewnewp)
ggsave("03_newnewnewp.pdf", width=4, height=3, units="in", scale=1.25)
newnewnewnewp <- run(newnewnewp); print(newnewnewnewp)
ggsave("04_newnewnewnewp.pdf", width=4, height=3, units="in", scale=1.25)
newnewnewnewnewp <- run(newnewnewnewp); print(newnewnewnewnewp)
ggsave("05_newnewnewnewnewp.pdf", width=4, height=3, units="in", scale=1.25)
newnewnewnewnewnewp <- run(newnewnewnewnewp); print(newnewnewnewnewnewp)
ggsave("06_newnewnewnewnewnewp.pdf", width=4, height=3, units="in", scale=1.25)



# The simulation, which depicted an organization
# comprised of eight levels--with
# 500 incumbents at the bottom and only 10 at
# the very top---began with an equal number
# of men and women awaiting promotion into
# one of the eight levels. Each person was
# assigned a performance evaluation score. The
# scores of men and women were distributed
# so as to be normal and identical (mu = 50, s = 10). 
# Incumbents with the highest scores became 
# eligible for promotion once a position
# was available. The simulation began by removing
# 15% of the incumbents. These positions
# were filled from within the organization,
# with eligible individuals (those with the highest scores)
# being promoted into the position. 
# The simulation continued to apply
# the 15% attrition rule until the organization
# was staffed entirely with "new" employees.
# That is, all individuals within the organization
# at the start of the simulation had been
# replaced with individuals from the initial
# pool. For each simulation, 20 computer runs
# were conducted to ensure an adequate degree
# of reliability.
# To assess the impact of male-female
# differences, "bias points" were added to the
# performance score of each man. In Simulation
# 1, 4.58 bias points, accounting for 5%
# of the variance in scores, were added; in
# Simulation 2, 2.01 bias points, accounting
# for 1% of the variance, were added. Proportion
# of variance was calculated by converting
# the standardized mean differences in performance
# scores between men and women
# into r 2. Given the standard deviation in the
# scores, the distributions of male and female
# scores were overlapping, even after the introduction
# of bias points. 


# as.vector(replicate(10000, sample(1:5, size=3, prob=1:5)))


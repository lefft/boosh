lefftpack::lazy_setup()
theme_set(theme_get() + theme(legend.title=element_text()))

# we are going to be using this dataset for getting corpus freq estimates
word_freq <- lefftpack::dataset_word_freq



### DEFINE THE DOMAIN AND SIMULATE A DATASET ----------------------------------

# these are the factors and levels (verbs this way nicer for plotting)
s <- c("sent_comp", "dir_obj")
t <- c(TRUE, FALSE)
v <- c("verb1\n[see]", "verb2\n[understand]", "verb3\n[realize]")

# so here is a df of all the conditions aka sentence types
sentence_types <- dplyr::data_frame(
  v = rep(v, each=4), 
  s = rep(s, each=2, times=3), 
  t = rep(t, times=6)
)


# eventually want to introduce 'bias' fields, e.g. .5 is SC bias; -.5 is DO
verbs <- data_frame(
  v = c("see", "understand", "realize")
) %>% 
  left_join(word_freq[, c("Word", "Frequency")], by=c(v="Word")) %>%
  mutate(v = paste0("verb", 1:3, "\n[", v, "]")) %>% 
  rename(freq=Frequency) %>% 
  # [THIS WILL BRING DOWN verb1 FREQ FOR SCALE REDUCTION -- REVISIT LATER]
  mutate(freq = ifelse(grepl("verb1", v), freq / 4, freq)) %>% 
  mutate(freq_rel  = freq / sum(word_freq$Frequency)) %>% 
  mutate(freq_norm = freq / sum(freq))

strucs <- data_frame(
  s = c("dir_obj", "sent_comp"), 
  freq = sum(verbs$freq) * c(.6, .4), 
  freq_rel = freq / sum(word_freq$Frequency), 
  freq_norm = freq / sum(freq)
)

thats <- data_frame(
  t = c(TRUE, FALSE), 
  freq = sum(verbs$freq) * c(.3, .7), 
  freq_rel = freq / sum(word_freq$Frequency), 
  freq_norm = freq / sum(freq)
)

# add field for `condition` (a unique combo of (v,s,t)) for quick handling
sentence_types <- sentence_types %>% 
  left_join(select(verbs,  v, v_freq=freq_norm), by="v") %>% 
  left_join(select(strucs, s, s_freq=freq_norm), by="s") %>%
  left_join(select(thats,  t, t_freq=freq_norm), by="t") %>% 
  mutate(token_prob = v_freq * s_freq * t_freq) %>% 
  mutate(condition = paste(
    paste0("v", stringr::str_extract(v, "\\d")), 
    case_when(s=="dir_obj" ~ "do", s=="sent_comp" ~ "sc"), 
    ifelse(t == TRUE, "that", "xthat"), sep="_"
  )) %>% select(v, s, t, v_freq, s_freq, t_freq, condition, token_prob)


# NOW: define weighting factor for each condition, for clearer RT differences.
#   want to create a multiplier so that we can make rt's related to condition.
#   just take token probs, add some noise, normalize, and subtract from one.
sentence_types$rt_weight <- with(
  sentence_types, 
  token_prob + rnorm(nrow(sentence_types), mean=0, sd=sd(token_prob)/2) # sd/2
) %>% (function(x) x / sum(x)) %>% (function(x) 1 - x)

# note: token probs normalized, but rt_weights are not (bc theyre multipliers)
sum(sentence_types$token_prob); sum(sentence_types$rt_weight)


# want to generate `num_tokens` fake data points -- "token" w only relevant info
num_tokens <- 1000

# create corpus and assign unique id to each token for later
samp_idx <- sample(
  1:nrow(sentence_types), 
  size=num_tokens, replace=TRUE, prob=sentence_types$token_prob
)
corpus <- sentence_types[samp_idx, ]

corpus$token_id <- paste0(
  "id", sprintf(paste0("%0", nchar(num_tokens), "d"), 1:num_tokens)
)

# this is also important -- where you specify empirical dist [**STRATIFY??**]
mean_rt <- 700
sd_rt <- 50

# now assign random rt's to each sentence, weighting by condition
# (dividing by mean weight gives nicer looking distribution ***REVISIT THO***)
corpus$reading_time <- 
  (rnorm(nrow(corpus), mean=mean_rt, sd=sd_rt) * corpus$rt_weight) %>% 
  (function(x) {x / mean(corpus$rt_weight) })



### SUMMARY STATS AND PLOTS OVER THE FAKE DATA --------------------------------

corpus_counts <- corpus %>% 
  group_by(v, s, t) %>% summarize(token_count = length(unique(token_id)))

corpus_stats <- corpus %>% group_by(v, s, t) %>% summarize(
  token_count = length(unique(token_id)), 
  grp_prob = unique(token_prob), 
  grp_weight  = unique(rt_weight), 
  rt_mean = mean(reading_time), 
  rt_sd   = sd(reading_time), 
  rt_min  = min(reading_time), 
  rt_max  = max(reading_time), 
  rt_ran  = rt_max - rt_min
) 


# try also swapping v, t, s for a different perspective 
ggplot(corpus_counts, aes(x=t, y=token_count, fill=s)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~v) + 
  labs(subtitle="**NEEDS TO reflect rare co-occurrence of dir_obj w that=TRUE")

melt(corpus, measure.vars=c("v","s","t")) %>% 
  ggplot(aes(x=reading_time, color=value)) + geom_density() + 
  facet_wrap(~variable, scales="free")

corpus_stats %>% ggplot(aes(x=s, y=rt_mean, fill=t)) + facet_wrap(~v) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(aes(label=round(grp_weight,2)), 
            position=position_dodge(width=.9), size=3, vjust=-.25) + 
  geom_text(aes(y=0, label=round(grp_prob,2)), 
            position=position_dodge(width=.9), size=3, vjust=-.25) + 
  theme(legend.title=element_text()) + 
  labs(x="sentence type ('s')", fill="presence of *that* ('t')", 
       caption=paste0("\nnumber at bottom of bar: prob of (s,v,t) combo",
                      "\nnumber at top of bar: rt weight of (s,v,t) combo"))

# "rt-plot-sept10-5pm-rtwt_halfsd.pdf" %>% ggsave(width=7, height=5, units="in")

# want a plot of token probability against RT
corpus %>% mutate(s = paste0(s, "\n"), t = paste0(t, "\n")) %>% 
  ggplot(aes(x=token_prob, y=reading_time, group=condition, color=v)) +
  geom_boxplot() + 
  geom_text(
    data=mutate(sentence_types, s = paste0(s, "\n"), t = paste0(t, "\n")), 
    aes(x=token_prob, y=0, label=condition, color=v), 
    angle=90, hjust=0, show.legend=FALSE
  )

# "rt-plot-sept10-rt-vs-prob_VERB.pdf" %>% ggsave(width=8, height=4, units="in")





### SOME PROBABILITY CALCULATIONS ---------------------------------------------

# load the funcs i wrote for this analysis
source("adaptation_functions.r")

# now want to simplify representation of verbs for quicker fiddling
corpus$v <- gsub("\\\n.*", "", corpus$v)

# in the paper it is stated that in general: 
#   "The joint probability of syntactic structures, verbs, and 
#    complementizer presence p(S,T,V) can be factored" [as:]
# 
#       p(s, t, v) = p(t | v, s) * p(s| v) * p(v)


# here we confirm w the fake data that this is indeed the case
results <- data_frame(
  v=gsub("\\\n.*","",sentence_types$v), s=sentence_types$s, t=sentence_types$t,
  lhs=rep(NA, nrow(sentence_types)), rhs=rep(NA, nrow(sentence_types)), 
  condition=sentence_types$condition
)

for (x in seq(len=nrow(results))){
  results$lhs[x] <- get_lhs(s=results$s[x], t=results$t[x], v=results$v[x])
  results$rhs[x] <- get_rhs(s=results$s[x], t=results$t[x], v=results$v[x])
}

c(`sum of lhs over all possibilities: ` = sum(results$lhs), 
  `sum of rhs over all possibilities: ` = sum(results$rhs))

results %>% ggplot(aes(x=lhs, y=rhs, color=v)) + geom_point()

results %>% ggplot(aes(x=condition, y=lhs, color=v)) + 
  geom_bar(stat="identity", fill="transparent") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(title="joint probability of triples w form (verb, construction, 'that')")

ggsave("out/joint-prob-plot.pdf", width=7, height=5, units="in")

# "in the subset [s==dir_obj & t==TRUE], ~half of verbs are 'see'"
prob_givenmult(event=list("v", "verb1"), 
               given1=list("s", "dir_obj"), given2=list("t", TRUE))

# confirmed (note that this sums to 1)
corpus %>% filter(s == "dir_obj", t == TRUE) %$%  
  table(v, useNA="ifany") %>% prop.table()


sapply(unique(corpus$v), function(x){
  prob_givenmult(event=list("v", "verb1"), 
                 given1=list("s", "dir_obj"), given2=list("t", TRUE))
})


sapply(unique(corpus$v), function(verb){
  sapply(unique(corpus$s), function(struc){
    sapply(unique(corpus$t), function(that){
      prob_givenmult(event=list("v", verb), 
                     given1=list("s", struc), given2=list("t", that))
    })
  })
})


results %>% 
  left_join(mutate(sentence_types, v=gsub("\\\n.*", "", v))) %>%
  mutate_if(is.numeric, round, digits=4) %>% 
  write.csv("out/joint_probs.csv", row.names=FALSE)






# notes: 
# 
# A ~~> "scale" parameter (fixes magnitude/scale/"degree", but not shape)
# 
# 
# 
#  
#  "we regressed the negative log conditional probability against 
#   length-corrected reading times. This measure is known as surprisal"



### [SCRATCH AREA] ----------

if (FALSE){
  dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
  options("contrasts")
  model.matrix(~ a + b, dd)
  model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum"))
  model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum", b="contr.poly"))
  m.orth <- model.matrix(~a+b, dd, contrasts = list(a = "contr.helmert"))
  crossprod(m.orth) # m.orth is  ALMOST  orthogonal
}


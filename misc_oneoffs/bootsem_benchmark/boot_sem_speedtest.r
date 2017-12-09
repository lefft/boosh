lefftpack::lazy_setup()

### OLD FUNCTION
# lefftpack::boot_se
function (vec, se = TRUE, b = 1000, n = length(vec[!is.na(vec)]), 
          dig = 2) 
{
  message("this func is still in development! (works fine but not v flexible)")
  vec <- vec[!is.na(vec)]
  boots <- rep(NA, times = b)
  for (x in 1:b) {
    boots[x] <- mean(sample(vec, size = n, replace = TRUE))
  }
  out <- ifelse(se == TRUE, sd(boots), mean(boots))
  return(out)
}

### NEW FUNCTION [now in lefftpack:: source]
# boot_sem
function(vec, sem=TRUE, b=1000, 
         n=sum(!is.na(vec)), na.rm=TRUE){
  # remove missings if desired 
  if (na.rm){ vec <- vec[!is.na(vec)] }
  
  # get the means of `b` bootstrap resamples of `vec`
  boot_means <- replicate(b, mean(sample(vec, size=n, replace=TRUE)))
  
  # return boot SEM (sd of boot means) if sem==TRUE; else return mean of means
  return(ifelse(sem==TRUE, sd(boot_means), mean(boot_means)))
}


# interesting -- preallocated for-loop (in old version) slightly faster 
# on big n, but slower on small n...
runs <- 500
timez <- data.frame(old = rep(NA, runs), new = rep(NA, runs))

for (x in 1:runs){
  
  vec <- rnorm(4e3)
  vec[c(1,6,999,245,999)] <- NA
  
  # set.seed(6933)
  timez$old[x] <- 
    unname(system.time(oldold <- lefftpack::boot_se(vec))["elapsed"])
  
  # set.seed(6933)
  timez$new[x] <- 
    unname(system.time(noonoo <- boot_sem(vec))["elapsed"])
  # identical(oldold, noonoo)
}
cap <- paste0("old = boot SEM with preallocated for-loop\n",
              "new = boot SEM with base::replicate()")
timez %>% melt(NULL) %>% 
  ggplot(aes(x=value, fill=variable)) + geom_density(alpha=.5) + 
  scale_x_continuous(limits=c(.095, .25)) + 
  geom_label(data=NULL, aes(x=.175, y=20, label=cap),
             show.legend=FALSE, inherit.aes=FALSE) +
  labs(x="", y="") 
# this is cleaner actually, but i likes curvie boxxxe from label geom 
# annotate("text", x=.2, y=40, label = "italic(R) ^ 2 == 0.75", parse = TRUE)

ggsave("../../../misc_oneoffs/bootsem_benchmark/speed_plot.pdf", 
       width=.5, height=.4, units="in", scale=10)

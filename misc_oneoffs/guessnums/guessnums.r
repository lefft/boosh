lefftpack::lazy_setup(); library("gridExtra")

# how long it take to guess a number from 1 to ...
upper_bound_exp <- 4
num_runs <- 500

# load this func, and then jump to bottom and call w desired params
run <- function(upper_bound_exp, num_runs, track_tries){
  # ensure we're using the same vals each time (restart session before run)
  set.seed(1234)
  uppers <- sample(10^(1:upper_bound_exp), size=num_runs, replace=TRUE)
  
  dat <- dplyr::data_frame(
    n_digits  = nchar(as.character(uppers)), 
    upper     = uppers, 
    value     = sapply(upper, function(x) sample(1:x, size=1)), 
    n_guesses = NA,
    time      = NA
  )
  print(dat)
  init_time <- Sys.time()
  
  for (x in 1:nrow(dat)){
    guess <- sample(1:dat$upper[x], size=1)
    counter <- 1
    tried <- c()
    start_time <- Sys.time()
    while (guess != dat$value[x]){
      if (track_tries){
        tried <- c(tried, guess)
      } 
      guess <- sample(setdiff(1:dat$upper[x], tried), size=1)
      counter <- counter + 1
    }
    dat$n_guesses[x] <- counter
    dat$time[x] <- 
      as.numeric(round(difftime(Sys.time(), start_time, units="secs"), 5))
    message(paste0("done guessing number ", x))
    message(paste0("guessed ", dat$value[x], " in ", dat$time[x], " seconds"))
    message(paste0("  >> it took ", dat$n_guesses[x], " guesses\n"))
  }
  
  ttime <- as.numeric(round(difftime(Sys.time(), init_time, units="mins"), 2))
  pnam <- paste0("track_guesses_",track_tries,"_",nrow(dat),"nums_",ttime,"min")
  
  boosh <- grid.arrange(
    ggplot(dat, aes(x=time)) + geom_density() + 
      facet_wrap(~factor(n_digits), scales="free_y") + 
      scale_x_continuous(limits=c(0, 30)), #10*upper_bound_exp)),
    ggplot(dat, aes(x=value, y=n_guesses, color=factor(n_digits))) + 
      geom_point(position="jitter") + 
      scale_x_continuous(limits=c(0, 10^upper_bound_exp)) + 
      scale_y_continuous(limits=c(0, (10^upper_bound_exp)*(upper_bound_exp-1))),
    ggplot(dat, aes(x=n_guesses, y=time, color=factor(n_digits))) + 
      geom_point(position="jitter") + theme(legend.position="none") + 
      scale_x_continuous(limits=c(0,(10^upper_bound_exp)*(upper_bound_exp-1))) +
      scale_y_continuous(limits=c(0, 30)), #10*upper_bound_exp)),
    nrow=3, top=pnam
  )
  print(boosh)
  
  ggsave(plot=boosh,filename=paste0("p_",pnam,".pdf"),
         width=9,height=9,units="in")
  write.csv(arrange(dat, desc(time)), 
            paste0("d_", pnam, ".csv"), row.names=FALSE)
}


run(upper_bound_exp, num_runs, track_tries=TRUE)
run(upper_bound_exp, num_runs, track_tries=FALSE)


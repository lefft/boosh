###### some lil simze adapted from a cv post on interp of ci's ################
# https://stats.stackexchange.com/questions/26450/
# why-does-a-95-confidence-interval-ci-not-imply-a-95-chance-of-containing-the
lefftpack::lazy_setup()


range_size <- 50

ns <- 1:range_size #rep(10, range_size) # 
mus <- rep(70, range_size)
variances <- rep(25, range_size)#seq(from=10, by=1, length.out=range_size) # 

# folder <- "ci_interp_sims_vary-var"
folder <- "ci_interp_sims_vary-n"
if (!dir.exists(folder)){ dir.create(folder) }

frames_per_sec <- 10
giffie_outname <- paste0(folder, ".gif")


delete_pngs <- TRUE







for (x in seq_along(ns)){
  
  replicates <- 1000
  
  n <- ns[x]
  mu <- mus[x]
  variance <- variances[x]
  
  xlims <- c(mean(mus) - round(.15*mean(mus)), mean(mus) + round(.15*mean(mus)))
  # xlims <- c(mu-10, mu+10)
  ylims <- c(0, .5)
  annnepos <- c(x=max(xlims), y=max(ylims))
  
  
  sigma <- sqrt(variance)
  
  conf.int.low <- rep(NA, replicates)
  conf.int.high <- rep(NA, replicates)
  conf.int.check <- rep(NA, replicates)
  
  #i=1
  for (i in 1:replicates) {
    
    
    sample <- rnorm(n, mu, sigma)
    se.mean <- sigma/sqrt(n)
    sample.avg <- mean(sample)
    prob <- 0.95
    alpha <- 1-prob
    q.alpha <- qnorm(1-alpha/2)
    low.95 <- sample.avg - q.alpha*se.mean
    high.95 <- sample.avg + q.alpha*se.mean
    
    conf.int.low[i] <- low.95
    conf.int.high[i] <- high.95
    conf.int.check[i] <- low.95 < mu & mu < high.95
  }
  
  # Collect the intervals in a data frame
  ci.dataframe <- data.frame(
    LowerCI=conf.int.low, UpperCI=conf.int.high, Valid=conf.int.check)
  
  
  # Take a peak at the top of the data frame
  # head(ci.dataframe)
  # str(ci.dataframe)
  
  # What fraction of the intervals included the true mean?
  ci.fraction <- length(which(conf.int.check, useNames=TRUE))/replicates
  ci.fraction
  
  
  # plot(density(ci.dataframe$LowerCI))
  # lines(density(ci.dataframe$UpperCI))
  annne <- paste0("n = ", n, "\nmu = ", round(mu), "\nvar = ", round(variance))
  
  ci.dataframe %>% melt("Valid") %>% 
    ggplot(aes(x=value, fill=variable)) + 
    geom_density(alpha=.5, show.legend=FALSE) + 
    geom_vline(data=NULL, aes(xintercept=mu)) + 
    labs(x="", y="") + 
    scale_x_continuous(limits=xlims) + scale_y_continuous(limits=ylims) + 
    annotate("label", x=annnepos["x"], y=annnepos["y"], label=annne, 
             vjust=1.1, hjust=1.1) 
  
  pname <- paste0(folder, "/plot",
                  "_n", sprintf("%02d", n), 
                  "_mu", sprintf("%03d", round(mu)), 
                  "_var", sprintf("%03d", round(variance)), 
                  ".png")
  ggsave(pname, width=2, height=1.5, units="in", scale=2.75)
  
  
  message("done with n = ", n)
  
}



library("magick")

dir(folder, pattern=".png$", full.names=TRUE) %>% 
  image_read() %>% 
  image_scale("x300") %>% 
  image_join() %>%
  image_animate(fps=frames_per_sec) %>% 
  image_write(path=giffie_outname, quality=10)

if (delete_pngs){ lapply(dir(folder, full.names=TRUE), unlink) }




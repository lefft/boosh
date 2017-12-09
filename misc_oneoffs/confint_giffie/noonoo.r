lefftpack::lazy_setup(sparse_grid=TRUE)


range_size_n <- 10
range_size_var <- 5
replicates <- 10000

ns <- 1:range_size_n     # rep(10, range_size) 
variances <- seq(from=20, by=20, length.out=range_size_var) 

# folder <- "ci_interp_sims_vary-n-var"     # n varies within var
folder <- "ci_interp_sims_vary-var-n"   # var varies within n
if (!dir.exists(folder)){ dir.create(folder) }


frames_per_sec <- 5
giffie_outname <- paste0(folder, ".gif")


delete_pngs <- TRUE



mu <- 70
xlims <- c(mu-10, mu+10)
ylims <- c(0, .5)
annnepos <- c(x=max(xlims), y=max(ylims))


# loop order doesnt matter, just the filenames! 
# xtheny <- sort(unlist(sapply(1:10, function(x) sapply(1:3, function(y) print(paste0("(x=", x, ", y=", y, ")"))))))
# ythenx <- sort(unlist(sapply(1:3, function(y) sapply(1:10, function(x) print(paste0("(x=", x, ", y=", y, ")"))))))
# identical(xtheny,ythenx)



for (var_idx in seq_along(variances)){
  
  variance <- variances[var_idx]
  
  for (n_idx in seq_along(ns)){
    
    n <- ns[n_idx]
    
    sigma <- sqrt(variance)
    
    conf.int.low <- rep(NA, replicates)
    conf.int.high <- rep(NA, replicates)
    conf.int.check <- rep(NA, replicates)
    
    
    for (i in 1:replicates) {
      
      # TO HAVE n VARY WITHIN VAR 
      if (folder=="ci_interp_sims_vary-n-var"){
        pname <- paste0(folder, "/plot",
                        "_var", sprintf("%03d", round(variance)),
                        "_n", sprintf("%02d", n), 
                        "_mu", sprintf("%03d", round(mu)), ".png")
      }
      
      # TO HAVE var VARY WITHIN n  
      if (folder=="ci_interp_sims_vary-var-n"){
        pname <- paste0(folder, "/plot",
                        "_n", sprintf("%02d", n), 
                        "_var", sprintf("%03d", round(variance)),
                        "_mu", sprintf("%03d", round(mu)), ".png")
      }
      
      
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
    annne <- paste0("n = ", sprintf("%03d", n), 
                    "\nmu = ", sprintf("%03d", round(mu)), 
                    "\nvar = ", sprintf("%03d", round(variance)))
                    # ifelse(nchar(variance)==2, 
                    #        paste0(" ", round(variance)), round(variance)))
    
    
    ci.dataframe %>% melt("Valid") %>% 
      ggplot(aes(x=value, fill=variable)) + 
      geom_density(alpha=.5, show.legend=FALSE) + 
      geom_vline(data=NULL, aes(xintercept=mu)) + 
      labs(x="", y="") + 
      scale_x_continuous(limits=xlims) + scale_y_continuous(limits=ylims) + 
      annotate("label", x=annnepos["x"], y=annnepos["y"], label=annne, 
               vjust=1.1, hjust=1.1) 
    
    
    ggsave(pname, width=5, height=3.5, units="in")#, scale=2)
    
    
    message("done with n = ", n)
    
  }
  
}

library("magick")

dir(folder, pattern=".png$", full.names=TRUE) %>% 
  image_read() %>% 
  image_scale("x300") %>% 
  image_join() %>%
  image_animate(fps=frames_per_sec) %>% 
  image_write(path=giffie_outname, quality=10)

if (delete_pngs){ lapply(dir(folder, full.names=TRUE), unlink) }
unlink(paste0(folder, "/"), recursive=TRUE)


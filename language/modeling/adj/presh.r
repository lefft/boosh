lefftpack::lazy_setup(sparse_grid=TRUE)
# hyperparameters 
n <- 1000
ht_min <- 60
ht_max <- 80
mean_ht <- 70
sd_ht <- 3
ht_gran <- 1
alpha_ <- 2

# space of utterances and states of the world 
u_space <- c("tall", "not_tall", "nothing")
A_space <- seq(from=ht_min, to=ht_max, by=ht_gran)

# priors for the A's and for the u's 
u_prior <- setNames(rep(1/length(u_space), length(u_space)), u_space) 
A_prior <- setNames(dnorm(A_space, mean_ht, sd_ht) / 
                      sum(dnorm(A_space, mean_ht, sd_ht)), nm=A_space)
th_prior <- setNames(dunif(A_space, ht_min, ht_max) / 
                       sum(dunif(A_space, ht_min, ht_max)), nm=A_space)



# NOTE: w these hyperparams, ~90% of points are btwn 5'6" and 6'6"
# xx <- rnorm(1e5, mean=mean_ht, sd=sd_ht)
# (sum(xx > 66 & xx < 78)) / length(xx)

#### RSA FUNCS FOR SIMS -------------------------------------------------------
normalize <- function(values) values / sum(values) 

make_mat <- function(A_space, u_space, theta){
  as.data.frame.matrix(sapply(u_space, function(u){
    sapply(A_space, function(A) meaning(u=u, ht=A, th=theta))
  })) %>% mutate(A = A_space) %>% select(A, one_of(u_space))
}

cost <- function(u){switch(u, tall=2, not_tall=2, nothing=1)}

meaning <- function(u, ht, th){
  relation <- switch(u, tall=`>=`, not_tall=`<`, nothing=function(...) TRUE)
  return(relation(ht, th))
}

L0_prob_A_given_u_th <- function(A, u, theta){
  semmat <- make_mat(A_space, u_space, theta=theta)
  A_given_u_theta <- A_prior
  A_given_u_theta[as.character(semmat$A[semmat[[u]]==FALSE])] <- 0
  A_given_u_theta_norm <- normalize(A_given_u_theta)
  return(A_given_u_theta_norm[as.character(A)])
}

utility <- function(u, A, theta){
  out <- log(L0_prob_A_given_u_th(A, u, theta)) - cost(u)
  if (is.infinite(out) | is.na(out) | is.nan(out))
    return(0) else return(out)
}

S1_prob_u_given_A_th <- function(u, A, theta){
  numerator <- exp(alpha_ * utility(u, A, theta))
  denominator <- sum(sapply(u_space, function(u_prime){
    exp(alpha_ * utility(u_prime, A, theta))
  }))
  return(numerator / denominator)
}

L1_prob_A_th_given_u_numerator <- function(A, theta, u){
  numerator <- 
    S1_prob_u_given_A_th(u, A, theta) * 
    A_prior[as.character(A)] * 
    th_prior[as.character(theta)]
  # message("dont have normalizing constant yet, just returning numerator")
  return(numerator)
}

#### END FUNCS ----------------------------------------------------------------




#### JUST BLINDLY APPLY EVERY FUNC TO EVERY COMBO OF THE RELEVANT PARAMS ------
apply_f <- function(f){
  as.data.frame.matrix(
    sapply(u_space, function(u){
      sapply(A_space, function(theta){
        sapply(A_space, function(A){
          f(A=A, u=u, theta=theta)
        })
      })
    })
  )
}

funxe <- c(L0_prob_A_given_u_th=L0_prob_A_given_u_th, 
           utility=utility, 
           S1_prob_u_given_A_th=S1_prob_u_given_A_th, 
           L1_prob_A_th_given_u_numerator=L1_prob_A_th_given_u_numerator)

## THIS OBJ MIIIIIGHT HAVE ALMOST EVERYTHING, JUST NEED TO FIGGER OUT WHAWHA
applied_fs <- lapply(funxe, apply_f)

labze <- paste0(expand.grid(A_space, A_space)$Var1, "_", 
                expand.grid(A_space, A_space)$Var2)

dd <- applied_fs %>% lapply(function(df) mutate(df, valze=labze)) %>% 
  (function(dfl) do.call("rbind", dfl)) %>% 
  mutate(func = rownames(.)) %>% 
  tidyr::separate(valze, into=c("h1", "h2")) %>% 
  melt(id.vars=c("h1","h2","func")) %>% 
  mutate(func = gsub("\\.\\d*", "", func))



ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + 
  geom_point() + 
  geom_vline(xintercept=3)


### plot the blob for 'tall' 
dd %>% 
  filter(variable=="tall") %>% 
  ggplot(aes(x=as.numeric(h1), y=value, color=as.numeric(h2))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'tall' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_tall-x-h1-color-h2.pdf", 
       width=10, height=8, units="in")

dd %>% 
  filter(variable=="tall") %>% 
  ggplot(aes(x=as.numeric(h2), y=value, color=as.numeric(h1))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'tall' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_tall-x-h2-color-h1.pdf", 
       width=10, height=8, units="in")



### plot the blob for 'not tall'
dd %>% 
  filter(variable=="not_tall") %>% 
  ggplot(aes(x=as.numeric(h1), y=value, color=as.numeric(h2))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'not tall' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_not-tall-x-h1-color-h2.pdf", 
       width=10, height=8, units="in")

dd %>% 
  filter(variable=="not_tall") %>% 
  ggplot(aes(x=as.numeric(h2), y=value, color=as.numeric(h1))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'not tall' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_not-tall-x-h2-color-h1.pdf", 
       width=10, height=8, units="in")


### plot the blob for 'nothing' 
dd %>% 
  filter(variable=="nothing") %>% 
  ggplot(aes(x=as.numeric(h1), y=value, color=as.numeric(h2))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'nothing' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_nothing-x-h1-color-h2.pdf", 
       width=10, height=8, units="in")

dd %>% 
  filter(variable=="nothing") %>% 
  ggplot(aes(x=as.numeric(h2), y=value, color=as.numeric(h1))) + 
  geom_vline(xintercept=70, color="#cc5500", size=1.5, linetype=1, alpha=.25) + 
  geom_point(position="jitter") + 
  facet_wrap(~func, scales="free_y") + 
  labs(caption="for 'nothing' only, vertical line marks mean height") + 
  theme(legend.key.width=unit(1, "inches"), legend.title=element_text())

ggsave("apply_everything_nothing-x-h2-color-h1.pdf", 
       width=10, height=8, units="in")

# END BLINDLY APPLY FUNCS -------------------



















  
  
  
  
##################################### TRY TO SAMPLE FROM POSTERIORS -----------
heights <- sample(A_space, size=n, replace=TRUE, prob=A_prior)
thetas  <- sample(A_space, size=n, replace=TRUE, prob=th_prior)
u_space <- u_space

ht_prior_samp <- function() sample(heights, size=1)
th_prior_samp <- function()  sample(thetas,  size=1)

# priors for heights and thetas [via specified prob densities]
pdf(paste0("priors_", length(A_space), "hts.pdf"), width=9, height=6)
plot(A_space, A_prior, col="blue"); points(A_space, th_prior, col="red")
legend("topright", 1:2, c("height (A)", "theta_tall"), 
       col=c("blue","red"), ncol=2, pch=1)
dev.off()



### NOW RUN SOME SIMS TO TRY TO SAMPLE FROM POSTERIORS... ~~*~~*~~*~~*~~*~~*~~*
n_iters <- 300

post_A  <- matrix(nrow=n_iters, ncol=length(A_space), 
                  dimnames=list(1:n_iters, A_space))
post_theta <- matrix(nrow=n_iters, ncol=length(A_space), 
                     dimnames=list(1:n_iters, A_space))

for (A in A_space){
  for (x in 1:n_iters){
    th_sample <- th_prior_samp()
    
    post_A[x, as.character(A)] <- 
      L1_prob_A_th_given_u_numerator(A, theta=th_sample, u="tall") / 
      sum(sapply(A_space, function(A_prime){
        L1_prob_A_th_given_u_numerator(A_prime, theta=th_sample, u="tall")
      }))
    if (x %% 50 == 0) message("  iter ", x, " of ", n_iters)
  }
  message("done with A = ", A, " at ", format(Sys.time(), "%X"))
  # message("mean value is ", mean(post_A[, as.character(A)]))
}


for (theta in A_space){
  for (x in 1:n_iters){
    ht_sample <- ht_prior_samp()
    
    post_theta[x, as.character(theta)] <- 
      L1_prob_A_th_given_u_numerator(ht_sample, theta, u="tall") / 
      sum(sapply(A_space, function(theta_prime){
        L1_prob_A_th_given_u_numerator(ht_sample, theta_prime, u="tall")
      }))
    if (x %% 50 == 0) message("  iter ", x, " of ", n_iters)
  }
  message("done with theta = ", theta, " at ", format(Sys.time(), "%X"))
  # message("mean value is ", mean(post_theta[, as.character(theta)]))
}

# post_Aold <- post_A
post_A <- as.data.frame.matrix(post_A)
# post_theta <- post_theta
post_theta <- as.data.frame.matrix(post_theta)

write.csv(post_A, paste0("post_A-10ht-", n_iters, "iter.csv"), row.names=FALSE)
write.csv(post_theta, paste0("post_theta-10ht-", n_iters, "iter.csv"), 
          row.names=FALSE)

# as_data_frame(head(post_A)) # <~~~ for easier quick view



### MAKE SUMMARY PLOT FOR POST A 
post_A_long <- post_A %>%
  melt(NULL, variable.name="ht") %>% 
  mutate(ht = as.numeric(as.character(ht)))


post_A_long %>% 
  ggplot(aes(x=ht, y=value, group=ht)) + geom_boxplot()

ggsave(paste0("post_A-boxes_10ht-", n_iters, "iter.pdf"))

# not informative bc spike at zero...
# post_A_long %>% ggplot(aes(x=value, group=ht)) + geom_density()

funcs <- c(min=min, median=median, max=max, mean=mean, 
           trim_mean10 = function(x) mean(x, trim=.1), 
           trim_mean25 = function(x) mean(x, trim=.25), 
           sd=sd, n=length, lu=lu)

# `lu=lefftpack::lu` can also be very useful, and `n=length` as refresher 
# funcs <- c(min=min, median=median, max=max, mean=mean, sd=sd, sum=sum)
# sapply(funcs, function(f) sapply(post_A, function(col) f(col)))



# `lu=lefftpack::lu` can also be very useful, and `n=length` as refresher 
sapply(funcs, 
       function(f) sapply(post_A, function(col) round(f(col), 6))) %>% 
  cbind(ht=A_space) %>% as_data_frame() %>% 
  melt(id.vars="ht", variable.name="stat") %>% 
  # first scale point throwing off min 
  filter(ht!=min(ht)) %>% 
  ggplot(aes(x=factor(ht), y=value)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~factor(stat), scales="free_y") + 
  labs(x="", y="", caption=paste0("post_A, n = ", n_iters, " iters"))

# v interesting... 
ggsave(paste0("post_A_plot-", n_iters, "iters.pdf"), 
       width=6, height=4, units="in", scale=1.5)




### DO SAME THING FOR THETA 
post_theta_long <- post_theta %>%
  melt(NULL, variable.name="ht") %>% 
  mutate(ht = as.numeric(as.character(ht)))

post_theta_long %>% 
  ggplot(aes(x=ht, y=value, group=ht)) + geom_boxplot()

ggsave(paste0("post_theta-boxes_10ht-", n_iters, "iter.pdf"))

# post_theta_long %>% 
#   ggplot(aes(x=value, group=ht)) + geom_density()


funcs <- c(min=min, median=median, max=max, mean=mean, 
           trim_mean10 = function(x) mean(x, trim=.1), 
           trim_mean25 = function(x) mean(x, trim=.25), 
           sd=sd, n=length, lu=lu)

sapply(c(min=min, median=median, max=max, mean=mean, sd=sd, sum=sum), 
       function(f) sapply(post_theta, function(col) round(f(col), 6))) %>% 
  cbind(ht=A_space) %>% as_data_frame() %>% 
  melt(id.vars="ht", variable.name="stat") %>% 
  # first scale point throwing off min 
  filter(ht!=min(ht)) %>% 
  ggplot(aes(x=factor(ht), y=value)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~factor(stat), scales="free_y") + 
  labs(x="", y="", caption=paste0("post_A, n = ", n_iters, " iters"))

# v interesting... 
ggsave(paste0("post_theta_plot-", n_iters, "iters.pdf"), 
       width=6, height=4, units="in", scale=1.5)








### NOW MAKE PRIORS PLOT BUT W POSTERIORS ADDED(????) ----
# NOTE -- MITE WANNA MAKE YMAX GLOBAL ACROSS THE TWO PLOTS??? 
A_postMN <- post_A_long %>% 
  group_by(ht) %>% summarize(value = mean(value)) %$% setNames(value, ht)

th_postMN <- post_theta_long %>% 
  group_by(ht) %>% summarize(value = mean(value)) %$% setNames(value, ht)




ymax <- max(c(A_prior, th_prior, A_postMN, th_postMN))

pdf(paste0("pre_vs_post_means-", n_iters, "iters.pdf"), width=9, height=6)
plot(NA, xlab="height", ylab="prob density", xlim=c(min(A_space), max(A_space)), ylim=c(0, ymax))
points(A_space, A_prior, lty=3, pch=1, type="b", col="blue") 
points(A_space, th_prior, lty=3, pch=1, type="b", col="red")
points(A_space, A_postMN, lty=1, pch=19, type="b", col="blue") 
points(A_space, th_postMN, lty=1, pch=19, type="b", col="red")
legend("topleft", 1:4, c("ht_prior", "th_prior", "ht_post", "th_post"), 
       col=c("blue","red", "blue","red"), ncol=2, pch=c(1,1,19,19))
dev.off()



A_postMDN <- post_A_long %>% 
  group_by(ht) %>% summarize(value = median(value)) %$% setNames(value, ht)

th_postMDN <- post_theta_long %>% 
  group_by(ht) %>% summarize(value = median(value)) %$% setNames(value, ht)

ymax <- max(c(A_prior, th_prior, A_postMDN, th_postMDN))

pdf(paste0("pre_vs_post_medians-", n_iters, "iters.pdf"), width=9, height=6)
plot(NA, xlab="height", ylab="prob density", xlim=c(min(A_space), max(A_space)), ylim=c(0, ymax))
points(A_space, A_prior, lty=3, pch=1, type="b", col="blue") 
points(A_space, th_prior, lty=3, pch=1, type="b", col="red")
points(A_space, A_postMDN, lty=1, pch=19, type="b", col="blue") 
points(A_space, th_postMDN, lty=1, pch=19, type="b", col="red")
legend("topleft", 1:4, c("ht_prior", "th_prior", "ht_post", "th_post"), 
       col=c("blue","red", "blue","red"), ncol=2, pch=c(1,1,19,19))
dev.off()




### v cute lol code :)
# `lu=lefftpack::lu` can also be very useful, and `n=length` as refresher 
# sapply(c(min=min, median=median, max=max, mean=mean, sd=sd, sum=sum), 
#        function(f) sapply(post_A, function(col) round(f(col), 6))) %>% 
#   cbind(ht=A_space) %>% as_data_frame %>% melt(id.vars="ht", var="stat")






# 
# 
# sapply(A_space, function(A){
#   sapply(u_space, function(u){
#     utility(u, A, theta=72)
#   })
# }) %>% t
# 

# sapply(A_space, function(height){
#   L1_prob_A_th_given_u_numerator(height, theta, u="tall") / 
#     sum(sapply(A_space, function(A){
#       L1_prob_A_th_given_u_numerator(A, theta, u="tall")
#     }))
# })

# try to get the theta posterior 
# sapply(A_space, function(theta){
#   L1_prob_A_th_given_u_numerator(A, theta, u="tall") / 
#     sum(sapply(A_space, function(A){
#       L1_prob_A_th_given_u_numerator(A, theta, u="tall")
#     }))
# })
# 
# 

# an idea: for each possible combo of u, A, and theta, calculate 
#   L1_prob_A_th_given_u_numerator 
# and then figure out how to normalize later... 



### 



# ############ ALEX BLOCK 
# 
# theta_tall <- 70
# sigma_tall <- 2
# tall <- rnorm(1e4, mean=theta_tall, sd=sigma_tall)
# very <- rchisq(1e4, df=3) 
# 
# very_tall <- function(q){
#   xx <- density(tall+very)
#   res <- rep(0,length(q))
#   res[q>min(xx$x)] <- 
#     sfsmisc::integrate.xy(xx$x,xx$y,a=min(xx$x),b=q[q>min(xx$x)])
#   return(res)
# }
# 
# not_very_tall<-function(x)return(1-very_tall(x))
# not_tall<-function(q)(1-c(pnorm(q,theta_tall,sigma_tall)))


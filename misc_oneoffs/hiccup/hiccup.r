lefftpack::lazy_setup()
d <- read.csv("hiccup_data.csv", header=FALSE, col.names=c("n","ms")) 

head(d)

hist(d$ms)
plot(d$n, d$ms)


mean(d$ms)
cumsum(d$ms)


ggplot(d, aes(x=n, y=ms)) + 
  geom_point(alpha=.25) + 
  geom_smooth(method="loess", se=TRUE) + 
  geom_line(alpha=.1) + 
  theme(panel.grid=element_blank())



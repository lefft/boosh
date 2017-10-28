lefftpack::lazy_setup()
library("stringr")
# snt <- sentences[c(13, 26, 45)]
txt <- lefftpack::text_una_manifesto


# speed test comparison of base vs stringr detection funcs
reps <- seq(from=1000, to=5000, by=500)
cont <- dplyr::data_frame(
  reps=reps, basef=rep(NA, length(reps)), strrf=rep(NA, length(reps))
)
for (x in seq_along(reps)){
  bigchar <- rep(txt, reps[x])
  basef <- 'sum(grepl("industrial", bigchar))'
  strrf <- 'sum(str_detect(bigchar, "industrial"))'
  speed_test <- compare_speed(expr1=basef, expr2=strrf)
  cont$basef[x] <- speed_test[1]
  cont$strrf[x] <- speed_test[2]
  message("done with ", reps[x], " rep iteration")
}
cont %>% melt(id.vars="reps", value.name="seconds") %>% 
  ggplot(aes(x=reps, y=seconds, color=variable)) + geom_point()
ggsave(filename="string-detection-speed-test.pdf", 
       width=5, height=5, units="in")
# 





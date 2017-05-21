# here's an example of a simple random forest call, 
# using a dataset i know + love

# note randomForest() will throw error when: 
# 	- factors are coded as character
# 	- ...

library("randomForest")

# use usual formula syntax (allows interaction??!)
dat_rf <- randomForest(
  binresponse ~ newscale + newadjtype + stimtype, 
  data=dat, importance=TRUE, norm.votes=TRUE
)

# look at importance scores
importance(dat_rf)

# 89% accuracy
sum(dat$binresponse==dat_rf$predicted) / nrow(dat)


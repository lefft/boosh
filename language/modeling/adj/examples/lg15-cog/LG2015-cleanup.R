d=read.csv("LG2015-data-anonymous.csv", header=TRUE)

length(levels(factor(d$participantID_anonymous))) # subjects total: 507
length(d$participantID_anonymous) # 10260 data points

levels(d$language)
d=subset(d, language %in% c("English", "english+(north+american)", "Englis", "English.+", "english+", "Rnglish", "English.+"))
# only data from self-reported native English speakers

length(levels(factor(d$participantID_anonymous))) # 484 remaining
length(d$participantID_anonymous) # 9797 data points remaining

bad.workers = c()
workers = levels(factor(d$participantID_anonymous))
for (i in 1:length(workers)) {
  wd = subset(d, participantID_anonymous == workers[i])
  responses = wd$response
  if (all(responses == 'accept') || all(responses == 'reject')) {
    bad.workers = c(bad.workers, workers[i])
  }
}

d = subset(d, !(participantID_anonymous %in% bad.workers))

d=d[d$rt >= 3500,]  # remove 196 trials with really fast RT

length(levels(factor(d$participantID_anonymous))) # 440 remaining
length(d$participantID_anonymous) # 8710 data points remaining

# raw materials for the SDT analysis. 
xtabs(~ response + argtype + modal, data=d)

# Note that there is an error in the published paper: Acceptance in the Necessary/contingent condition was 0.35 [360/1024], rather than 0.41 as reported.

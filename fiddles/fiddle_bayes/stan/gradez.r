library("dplyr")
# http://gradedistribution.registrar.indiana.edu/
header <- readLines("data/reportID_gradedist-chem.csv", n=1) %>% 
  strsplit(split=",") %>% unlist() %>% (function(x) gsub("/", "", x)) %>% 
  (function(x) gsub("\"", "", x)) %>%   (function(x) gsub("%", "_pct", x)) %>% 
  (function(x) gsub("-", "min", x)) %>% (function(x) gsub("\\+", "pls", x)) %>% 
  c("empty") # add empty field bc there's trailing commas in the datasets

datc <- read.csv("data/reportID_gradedist-chem.csv", col.names=header) %>% 
  select(GPA.GRADES,DEPARTMENT,AVG.SECT.GPA,AVG.STDNT.CUM.GPA,contains("_pct"))

datp <- read.csv("data/reportID_gradedist-philos.csv", col.names=header) %>% 
  select(GPA.GRADES,DEPARTMENT,AVG.SECT.GPA,AVG.STDNT.CUM.GPA,contains("_pct"))

dat <- rbind(datc, datp)

clean_dat <- function(col){
  col <- as.character(col)
  col <- ifelse(col=="NOT AVAILABLE - SMALL CLASS SIZE", NA, col)
  col <- ifelse(col=="NR", NA, col)
  return(as.character(col))
}

dat <- as.data.frame(lapply(dat, clean_dat))
names(dat)
cols <- c("DEPARTMENT", "GPA.GRADES", "AVG.SECT.GPA","AVG.STDNT.CUM.GPA",
          "A_pct","B_pct","C_pct","D_pct")
dat <- dat[, cols]
dat$DEPARTMENT <- as.character(dat$DEPARTMENT)

for (x in seq_along(dat)){
  if (is.factor(dat[[x]])){
    dat[[x]] <- as.numeric(as.character(dat[[x]]))
  }
}


dat %>% group_by(DEPARTMENT) %>% summarize(
  meanA = mean(A_pct, na.rm=TRUE),
  meanB = mean(B_pct, na.rm=TRUE),
  meanC = mean(C_pct, na.rm=TRUE),
  meanD = mean(D_pct, na.rm=TRUE),
  studs = sum(GPA.GRADES, na.rm=TRUE),
  clsss = length(DEPARTMENT),
  stdpc = studs / clsss, 
  avgpa = mean(AVG.STDNT.CUM.GPA, na.rm=TRUE)
) %>% mutate_if(is.numeric, round, digits=2)


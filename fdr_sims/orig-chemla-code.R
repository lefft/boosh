Part <- 40
REP <- 1000

pvaluefromreplace <- function (M,C,D,ql,qh, replace=T) {
	if (ql) {QL <- quantile(D[[M]], ql)} else {QL <- min(D[[M]])-1}
	if (qh) {QH <- quantile(D[[M]], 1-qh)} else {QH <- max(D[[M]])+1}
	if (!replace) { DD <- subset(D, D[[M]]>QL & D[[M]]<QH) }
	else {
		DD <- D
		RL <- DD[[M]] < QL
			DD[[M]][RL] <- QL
		RH <- DD[[M]] > QH
			DD[[M]][RH] <- QH
		}
	a <- summary(aov(DD[[M]]~DD[[C]]))
	return(a[[1]][["Pr(>F)"]][1])	
	}

pvaluefrom <- function (M,C,D,R) {
	DD <- subset(D, R)
	a <- summary(aov(DD[[M]]~DD[[C]]))
	return(a[[1]][["Pr(>F)"]][1])	
	}




X <- data.frame(Cond = rep(1:2, each=Part/2))
RESULTS <- data.frame(simulation=NULL, replacement=NULL, differentmeasure=NULL, lowexclusion=NULL, highexclusion=NULL, pvalue=NULL)

for (x in 1:REP) {
#	print(x)
	X$RT <- rnorm(Part)
	X$EXCL <- rnorm(Part)
	
	for (ql in c(0, .025, .05, .075, .1)) { for (qh in c(0, .025, .05, .075, .1)) {
	
			L <- data.frame(simulation=x, replacement=TRUE, differentmeasure=FALSE, lowexclusion=ql, highexclusion=qh, 
				pvalue= pvaluefromreplace("RT", "Cond", X, ql, qh, replace=T)
				)
			RESULTS <- rbind(RESULTS, L)

			if (ql>0 | qh>0) {
				L <- data.frame(simulation=x, replacement=FALSE, differentmeasure=FALSE, lowexclusion=ql, highexclusion=qh, 
					pvalue= pvaluefromreplace("RT", "Cond", X, ql, qh, replace=F)
					)
				RESULTS <- rbind(RESULTS, L)

				L <- data.frame(simulation=x, replacement=FALSE, differentmeasure=TRUE, lowexclusion=ql, highexclusion=qh, 
					pvalue= pvaluefrom("RT","Cond",X,X$EXCL>quantile(X$EXCL,ql) & X$EXCL<quantile(X$EXCL,1-qh))
					)
				RESULTS <- rbind(RESULTS, L)
				}
	
	}}
	
}

# Single p-value
	SUBRES <- subset(RESULTS, lowexclusion==0 & highexclusion==0)
	with(SUBRES, table(pvalue<.05))
	with(SUBRES, quantile(pvalue, .05))
	
# Multiple thresholds, without replacement
	SUBRES <- subset(RESULTS, (replacement==FALSE & differentmeasure==FALSE) | (lowexclusion==0 & highexclusion==0))
	SUBRES <- aggregate(pvalue~simulation, min, data=SUBRES)
	with(SUBRES, table(pvalue<.05))
	with(SUBRES, quantile(pvalue, .05))

# Multiple thresholds, with replacement
	SUBRES <- subset(RESULTS, replacement==TRUE & differentmeasure==FALSE)
	SUBRES <- aggregate(pvalue~simulation, min, data=SUBRES)
	with(SUBRES, table(pvalue<.05))
	with(SUBRES, quantile(pvalue, .05))

# Multiple thresholds, different measure (without replacement)
	SUBRES <- subset(RESULTS, (replacement==FALSE & differentmeasure==TRUE) | (lowexclusion==0 & highexclusion==0))
	SUBRES <- aggregate(pvalue~simulation, min, data=SUBRES)
	with(SUBRES, table(pvalue<.05))
	with(SUBRES, quantile(pvalue, .05))









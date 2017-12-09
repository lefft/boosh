

### IMPLEMENTATION OF MULTINOMIAL NAIVE BAYES FOR TEXT CLASSIFICATION 
naive_bayes_pred <- function(docs, labels){
  # currently only for two-class problems 
  if (length(unique(labels)) != 2){
    message("can only do two-class problems! (returning NA)") 
    return(NA)
  }
  ### ORGANIZE AND RESHAPE DATA 
  # names of the two classes (for spam data, "ham" and "spam")
  lab_names <- sort(unique(labels))
  # organize the data for convenient reference below 
  corp <- data.frame(
    lab=labels, doc=docs, id=paste0("doc", sprintf("%02d", seq_along(docs))), 
    stringsAsFactors=FALSE
  )
  # and get the vocab and a dtm, which we will make reference to later 
  vocab <- unique(unlist(sapply(corp$doc, function(d) strsplit(d, split=" "))))
  # initialize a dtm w just a column for document id 
  dtm <- data.frame(id=corp$id, stringsAsFactors=FALSE)
  # expand the matrix by adding a column for each word in the vocab 
  for (x in seq_along(vocab)){
    dtm[[x+1]] <- NA_real_
    names(dtm)[x+1] <- vocab[x]
  }
  # fill in word counts for each document (matrix will be very sparse)
  for (x in seq_along(vocab)){
    dtm[[vocab[x]]] <- 
      stringr::str_count(corp$doc, pattern=paste0("\\b", vocab[x], "\\b"))
  }
  ### DEFINE COMPONENTS OF THE MODEL 
  # priors over the labels, the vocab words, and the actual documents 
  lab_prior <- function(lab){
    sum(corp$lab==lab, na.rm=TRUE) / sum(!is.na(corp$lab))
  }
  word_prior <- function(word){
    tokens <- unlist(sapply(corp$doc, strsplit, split=" ", USE.NAMES=FALSE))
    sum(tokens==word, na.rm=TRUE) / sum(!is.na(tokens)) 
  }
  doc_prior <- function(doc){
    words <- unlist(sapply(doc, strsplit, split=" ", USE.NAMES=FALSE))
    prod(sapply(words, word_prior))
  }
  # likelihood for a word is prop lab docs that word occurs in 
  word_likhood <- function(word, lab){
    lab_docs <- corp$id[corp$lab==lab] 
    # TODO: add ability to use tf-idf weighting, also freqs instead of 0/1+
    (sum(dtm$id %in% lab_docs & dtm[[word]] > 0) / length(lab_docs)) + 1e-6
  }
  # likelihood for a doc is the product of its component word likhoods 
  doc_likhood <- function(doc, lab){
    words <- unlist(sapply(doc, strsplit, split=" ", USE.NAMES=FALSE))
    prod(sapply(words, word_likhood, lab=lab))
  }
  # posterior is the doc likelihood multiplied by the label prior 
  doc_posterior <- function(doc, lab){
    prior <- lab_prior(lab)
    likhood <- doc_likhood(doc, lab) 
    (likhood * prior)
  }
  # decision rule is a special case of argmax with just two classes (i.e. >)
  pred_doc_lab <- function(doc){
    post_lab1 <- doc_posterior(doc, lab_names[1])
    post_lab2 <- doc_posterior(doc, lab_names[2])
    if (post_lab1 > post_lab2) return(lab_names[1]) else return(lab_names[2])
  }
  ### PREDICT LABELS 
  return(sapply(corp$doc, pred_doc_lab, USE.NAMES=FALSE))
}









# , fill=variable
# munsell::mnsl(c("2.5PB 2/4", "2.5PB 7/10"))
# munsell::mnsl(munsell::mnsl_hues()[1:5])
# mutate(variable = as.numeric(gsub("n_", "", variable))) %>%
# + scale_fill_gradient(low="lightgray", high="black") 
# scale_color_manual(values=munsell::mnsl(c("2.5R 2/4","5R 2/4","7.5R 2/4","10R 2/4","2.5YR 2/4","5YR 2/4","7.5YR 2/4","10YR 2/4","2.5Y 2/4","5Y 2/4","7.5Y 2/4","10Y 2/4","2.5GY 2/4","5GY 2/4","7.5GY 2/4","10GY 2/4","2.5G 2/4","5G 2/4","7.5G 2/4","10G 2/4")))

# tz fucked up for some reason, prob mac update smh :/
# message(Sys.time() + (60*60*18)) 


# can check results on the whole thing (not realistic sitch)
#   spam_labs <- dat$label
#   spam_preds <- naive_bayes_pred(docs=dat$text, labels=dat$label)
#   sum((spam_labs == spam_preds)/length(spam_labs))
# 
# if tt-split desired (need to make interface for out of sample predictions)
#   train_idx <- sample(1:nrow(dat), size=.75*nrow(dat))
#   train <- dat[train_idx, ]; test <- dat[-train_idx, ]



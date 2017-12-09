# some made up data w made up labels 
docs <- c(
  # positive sentences 
  "i like donut", "i love eggie", "everyone enjoy puppy espec me", 
  "i like puppies", "me like doggie yes me do", 
  # negative sentences 
  "i do not like kittie", "me no like sad no like", "no one enjoy burning",
  "i hate donut", "you no love no eggie", 
  # sentences w/o labels (artificial test set)
  "i enjoy dootza", "dootza no like spicy food", "dootza is me bebe"
)
corp <- data_frame(
  id = paste0("doc", sprintf("%02d", seq_along(docs))), 
  lab = c(rep(c("pos","neg"), each=5), rep(NA, 3)), 
  doc = docs
)
corp

vocab <- unique(unlist(sapply(corp$doc, function(d) strsplit(d, split=" "))))
vocab

dtm <- data_frame(id=corp$id)
for (x in seq_along(vocab)){
  dtm[[x+1]] <- NA_real_
  names(dtm)[x+1] <- vocab[x]
}
for (x in seq_along(vocab)){
  dtm[[vocab[x]]] <- 
    stringr::str_count(corp$doc, pattern=paste0("\\b", vocab[x], "\\b"))
}
dtm


# decision algo: assign 'neg' label if 
#   prob(neg | [w1,...,wn]) > prob(pos | [w1,...,wn]) 
# 
# priors are just proportions: |neg|/|docs| 
# 
# likhoods are calculated via product chaining: 
#   prob([w1,...,wn] | neg) = 
#     prob(w1 | neg) * ... * prob(wn | neg) = product_wi(prob(wi | neg)) 
# 
# posterior is product of prior and likhood 
# 
# 
# QUESTIONS: 
#   - so the "prob" of a doc is product of probs of its words? 
#   - how we gets normalizing constantte?!?! (smoothly) 
# 
# lab="neg"; word="like"; doc=corp$doc[1]

lab_prior <- function(lab){
  sum(corp$lab==lab, na.rm=TRUE) / sum(!is.na(corp$lab))
}
lab_prior("pos")
word_prior <- function(word){
  tokens <- unlist(sapply(corp$doc, strsplit, split=" ", USE.NAMES=FALSE))
  sum(tokens==word, na.rm=TRUE) / sum(!is.na(tokens)) 
}
doc_prior <- function(doc){
  words <- unlist(sapply(doc, strsplit, split=" ", USE.NAMES=FALSE))
  prod(sapply(words, word_prior))
}

word_likhood <- function(word, lab){
  # take all docs with label lab. what prop of them have word word? 
  lab_docs <- corp$id[corp$lab==lab]
  # [adding v small amount of prob for safe arithmetic w zeros]
  (sum(dtm$id %in% lab_docs & dtm[[word]] > 0) / length(lab_docs)) + 1e-6
}
doc_likhood <- function(doc, lab){
  words <- unlist(sapply(doc, strsplit, split=" ", USE.NAMES=FALSE))
  prod(sapply(words, word_likhood, lab=lab))
}

doc_posterior <- function(doc, lab){
  prior <- lab_prior(lab)
  likhood <- doc_likhood(doc, lab) 
  # norm const shd be sum of (likhood(d,lab) * prior(lab)) for all d(?!?!) 
  (likhood * prior)
}

pred_doc_lab <- function(doc){
  post_pos <- doc_posterior(doc, "pos")
  post_neg <- doc_posterior(doc, "neg")
  if (post_pos > post_neg) return("pos") else return("neg") 
}



setNames(sapply(corp$doc, pred_doc_lab), nm=corp$lab)




norm_const_pos <- sum(sapply(corp$doc, doc_likhood, lab="pos"))
norm_const_neg <- sum(sapply(corp$doc, doc_likhood, lab="neg"))
norm_const_doc9 <- sum(sapply(c("pos","neg"), doc_likhood, doc=corp$doc[9]))


# ends up as .5...?!?! 
(doc_posterior(corp$doc[9], "pos") / norm_const_doc9) + 
  (doc_posterior(corp$doc[9], "neg") / norm_const_doc9)

(doc_posterior(corp$doc[9], "pos") + doc_posterior(corp$doc[9], "neg")) /
  norm_const_doc9





# ### [THIS PART IS NOT NECESSARY BUT CAN BE NICE TO LOOK AT]
# vocab_df <- data_frame(
#   word = vocab, pos_count = NA_integer_, neg_count = NA_integer_
# )
# vocab_df$pos_count <- sapply(vocab, function(word){
#   sum(stringr::str_count(corp$doc[corp$lab=="pos"], 
#                          pattern=paste0("\\b", word, "\\b")))
# })
# vocab_df$neg_count <- sapply(vocab, function(word){
#   sum(stringr::str_count(corp$doc[corp$lab=="neg"], 
#                          pattern=paste0("\\b", word, "\\b")))
# })
# vocab_df$test_count <- sapply(vocab, function(word){
#   sum(stringr::str_count(corp$doc[is.na(corp$lab)], 
#                          pattern=paste0("\\b", word, "\\b")))
# })
# vocab_df$total_count <- 
#   vocab_df$pos_count + vocab_df$neg_count + vocab_df$test_count


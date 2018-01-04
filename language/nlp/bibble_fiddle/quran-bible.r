# lefftpack::lazy_setup()
lefftpack::quiet_attach("igraph", "ggraph")
lefftpack::lazy_setup(show_message=FALSE, set_plot_theme=FALSE)

mapply(pryr::partial(assign, envir=.GlobalEnv), 
       c("bible_url","quran_url"), readLines("urls.txt"))

get_clean_holy_book <- function(url, dest, vname){
  if (file.exists(dest)){
    message("already dl'ed brah, just gon re-read as `", vname, "` ¯\\_(ツ)_/¯")
    return(assign(vname, readLines(dest), envir=.GlobalEnv))
  }
  raw_book <- readLines(url, warn=FALSE) %>% paste(collapse="\n") %>% 
    (function(chr) gsub(".*<pre>(.*)</pre>.*", "\\1", chr))
  writeLines(raw_book, dest)
  assign(vname, readLines(dest), envir=.GlobalEnv)
  message("parsed text from url:\n  ", 
          ">> written to `", dest, "`,\n  ", ">> read as var `", vname, "`")
  # assumes `url` passed as vname (but `dest` arg isnt, so no need to `rm()`)
  rm(list=deparse(substitute(url)), envir=.GlobalEnv) 
}



get_clean_holy_book(bible_url, "bible.txt", "bible")
get_clean_holy_book(quran_url, "quran.txt", "quran")

quran <- quran[gsub(" ", "", quran) != ""]
bible <- bible[gsub(" ", "", bible) != ""]

# 3538: "Chapter 1 "              <~~~ quran starts here 
# 3539: "Ai-Fatihah The Opening "
quran <- quran[3538:length(quran)]

# 0164: "Genesis "                <~~~ bible starts here 
# 0165: ""
bible <- bible[164:length(bible)]

### book o job starts at 50760, ends at 77632 (after subsetting w 164)
### (72983, 77632 in raw)
job <- bible %>% 
  `[`(50760:53526) %>% 
  gsub(" +", " ", .) %>%
  gsub("(\\d+)", " <breakpoint> VERSE:\\1", .) %>% 
  paste(collapse=" ") %>% 
  strsplit(split=" <breakpoint> ") %>% 
  unlist() %>% 
  `[`(.!="Job  ")


d <- data_frame(verse = as.numeric(gsub("(^VERSE:)(\\d+)(.*)", "\\2", job)),
                text = gsub(" +", " ", gsub("^VERSE:\\d+ +", "", job)))

# surprisingly complicated to assign chapter guesses! 
new_chap = c(TRUE, sapply(2:length(d$verse), 
                          function(idx) d$verse[idx] < d$verse[idx-1]))
ch_starts <- which(new_chap)
ch_ends <- lead(which(new_chap), default=nrow(d)+1)-1 
ch_list <- mapply(`:`, ch_starts, ch_ends)

d$chap <- unlist(sapply(seq_along(ch_list), 
                        function(x) rep(x, times=length(ch_list[[x]]))))

d <- d[, c("chap","verse","text")]

# (problem is 42:17 is final verse of book so we're way off on chaps :/)
d # d %>% View

d$text %>% paste(collapse="\n") %>% writeLines("job-split.txt")

clean <- pryr::partial(scrub_doc, lower=TRUE, toss_punct=TRUE, toss_num=TRUE)
stops <- c("it", "we", "you", "them", "my", "they", "out", "our", "no", "not", 
           "would", "do", "he", "her", "she", "him", "us", "so", "if", "one",
           "that", "is", "him", "he", "them", "am", "its", "than", "other",
           "so",
           letters, get_stop_list("bible"))

job_bg <- text2bigram(remove_stops(clean(d$text), stops))
bib_bg <- text2bigram(remove_stops(clean(bible[1e3:1e4]), stops))
qrn_bg <- text2bigram(remove_stops(clean(quran[1e3:1e4]), stops))

bigram_plot(job_bg, "bigram", "count", top_n=100)
ggsave("job_bg.pdf", width=11, height=8.5, units="in")
bigram_plot(bib_bg, "bigram", "count", top_n=100)
ggsave("bib_bg.pdf", width=11, height=8.5, units="in")
bigram_plot(qrn_bg, "bigram", "count", top_n=100)
ggsave("qrn_bg.pdf", width=11, height=8.5, units="in")



# "(^VERSE:\\d+)(.*)" <~~~ try w regexpr()
# as.numeric(gsub("(^VERSE:)(\\d+)(.*)", "\\2", job))
# writeLines("job-split.txt")
# can get verse string w: `verse_str = gsub("(^VERSE:\\d+).*", "\\1", job)`

# "https://archive.org/stream/EnglishTranslationOfTheHolyQuran/trans-quran-web_djvu.txt"
# "https://archive.org/stream/NIVBible/NIV%20-%20Bible_djvu.txt"
# 
# 
# bible toc starts just after: http://GODoor.net 
# and ends just after:         Revelation
# 
# quran toc starts just after: The Holy Quran:*
# and ends just after:         747




# need to supply a df with a bigram column and a count column 
bigram_plot <- function(bigram_df, bg_col, count_col, 
                        top_n=nrow(bigram_df), seed=NULL, 
                        title="", text_size=5, vjust=1, hjust=1){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  if (nrow(bigram_df)==0) return(NULL)
  
  names(bigram_df)[names(bigram_df)==bg_col] <- "bigram"
  names(bigram_df)[names(bigram_df)==count_col] <- "count"
  
  bigram_df <- bigram_df[, c("bigram", "count")]
  bigram_df <- tidyr::separate(bigram_df, col=bigram, into=c("w1","w2"),sep=" ")
  
  bg_arrow <- grid::arrow(length=unit(.1, "inches"), ends="last", type="open")
  
  
  bigram_df %>% 
    dplyr::top_n(n=top_n, count) %>% 
    igraph::graph_from_data_frame() %>% 
    ggraph::ggraph(layout="nicely") + 
    ggraph::geom_node_point(color="darkgreen", size=1) + 
    ggraph::geom_node_text(aes(label=name), 
                           size=text_size, vjust=vjust, hjust=hjust) +
    ggraph::geom_edge_link(aes(edge_alpha=count), width=1, arrow=bg_arrow) + 
    theme_void() + 
    theme(legend.position="bottom") + 
    labs(title=ifelse(is.null(title), 
                      paste0(top_n, " most frequent bigrams"), title), 
         subtitle=ifelse(is.null(title), 
                         "", paste0(top_n, " most frequent bigrams")))
}

### setup ---------------------------------------------------------------------
# load dependencies
lefftpack::quiet_attach(
  "dplyr","magrittr","knitr","tidyr","tidytext","gutenbergr",
  "rvest","pdftools","ggplot2","igraph","ggraph","grid","gridExtra"
)
# load functions for this proj
source("../../oneoffs/nlp-foncs.r"); source("unagoog-funcs.r")
# want to save the plots?
save_piqqz <- FALSE; save_dir <- "docint_plots/"
# stoplist -- can use 'una', 'bible', 'snowball', 'onix', or 'SMART' 
slist <- "una"; stops <- stopze(stop_list=slist)

### acquire data --------------------------------------------------------------
# load all the docs
docs <- get_docs(folder="text/")
# and here's their names
nm <- doc_names()

# [FOR TESTING, JUST GET A FEW]
# docs <- docs[c("u","g","h")]
# nm   <- nm[c("u","g","h")]
# save_dir <- "una-goog-hinkie/"

### compute doc intersections + plot ------------------------------------------
if (!dir.exists(save_dir)){dir.create(save_dir)}
for (x in seq_along(docs)){
  for (y in seq_along(docs)){
    if (identical(docs[[x]], docs[[y]])){
      next()
    }
    
    message(paste0("computing intersection of docs:", 
                   "\n  >> ", nm[x], "\n  >> ", nm[y]))
    
    dint <- doc_intersection(docs[[x]], docs[[y]], nm[x], nm[y], stoplist=stops)
    if (is.null(dint)){
      message("there's no intersection, moving on </3")
      next()
    }
    dint_df <- dint$bigram_data
    dint_plot <- dint$bigram_plot
    int_size <- sprintf("%03d", nrow(dint_df))
    outname <- paste0(save_dir, int_size, "__", nm[x], "__", nm[y], ".pdf")
    # dont make plots if the intersection is empty
    if (int_size=="000"){
      next()
    }
    # dont save a plot if we already have one comparing the same docs
    if (file.exists(paste0(save_dir,int_size,"__",nm[y],"__",nm[x],".pdf"))){
      next()
    }
    # otherwise, save it
    ggsave(plot=dint_plot, filename=outname, height=8, width=10, units="in")
    write.csv(dint_df, gsub(".pdf", ".csv", outname), row.names=FALSE)
  }
}

### dev area ------------------------------------------------------------------
# these are what we got: 
u  <- docs$u    # unabomber manifesto
g  <- docs$g    # google diversity "echo-chamber" memo
h  <- docs$h    # sam hinkie resignation letter
m  <- docs$m    # timothy mcveigh letter about bombing
k  <- docs$k    # mlk "i have a dream" speech
b  <- docs$b    # breton "surrealism manifesto"
tp <- docs$tp   # trump press conference (feb2017)
ts <- docs$ts   # trump speech at a dinner (march2017)
tm <- docs$tm   # trump speech in miami (sept2016)


# dat <- text2bg_count(u)
# dat <- text2bg_count(g) # seems fishy...
# 
# dat %>% 
#   filter(!w1 %in% stops) %>% filter(!w2 %in% stops) %>% 
#   select(-bigram) %>% bigram_plot(top_n=20, remove_stops=TRUE, stops=stops)
# 
# ggsave("out/una-top20.pdf", width=10, height=8, units="in")



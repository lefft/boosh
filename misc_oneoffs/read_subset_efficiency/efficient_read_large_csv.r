#### A COMMON PROBLEM:
# you have a huge dataset w many columns, only a few of which you want. 
# you waste time by reading the whole thing, or by messing w read.csv args...
# 
# here i give a solution with read_selected_cols() and get_col_info_from_file()
# 
# 
# 
#### EXAMPLE + DEMO:  
# how long does it take to read in a handful of cols of a 1gb file??? 
# default way wastes a lot of resources reading in stuff that you're 
# just going to ultimately throw away. 

# TODO: 
#   - add ability to select cols by a pattern (e.g. ends with "blah")
#   - clean up ... stuff 
#   - introduce error handling in case that infer_from_row=10 is not 
#     enough to correctly infer columns 
#   - set NULL as default to read in all cols 
#   - change "fname"/"loc" to be uniform 
#   - deal w length.out arg better when specifying sizes 
#   - deal w transition btwn small steps and large steps (border of zoom box)
#   - figger out warnings  ": Unknown or uninitialised column: 'value'."
#   - better box size in many files case 
#   - ... 




# big file, w 442474 rows 
# "../../projjies/HMC_local/CDC-AMSM/repo/data_raw/lgbt_orgs_timelines.csv"
# here we compare: read all then slice --> find then read only relevant cols 
# 
# say we want to read the text/bio and user_id/name cols 
# (suppose we've figured out colnames in advance )

lefftpack::lazy_setup(set_plot_theme=TRUE, sparse_grid=FALSE)

wrk <- "../../../../work NORC/HMC/"

# nrows in the folder: 2084602, 2055254, 1843830, 1838358, 1846434, 1765665
# sapply(dir(loc, full.names=TRUE, pattern="\\.csv$"), function(file){
#   dim(read_selected_cols(file, keeper_cols="lang"))})
# 
# nrows in the "big" file: 32274 
# read_selected_cols(fname=fname, keeper_cols="user_id") %>% dim

# big_file <- TRUE
big_file <- FALSE

switchpoint <- 2000

upper_end <- ifelse(big_file, 30000, 100000)

the_tit <- ifelse(big_file, 
                  "performance comparison on 4 columns of 1gb dataset", 
                  "performance comparison on folder with many datasets")


#################################### 
# 
get_col_info_from_file <- function(fname, infer_from_nrow=10, ...){
  sapply(read.csv(fname, nrows=infer_from_nrow, ...), typeof)
}
# NOTE: if in the initial scan you don't see any non-missings, then read.csv 
# will end up just assuming 'logical', which if incorrect will cause an error
# (e.g. caused by 'bit.ly/pages/privacy')
read_selected_cols <- function(fname, keeper_cols, ...){
  col_types <- get_col_info_from_file(fname, stringsAsFactors=FALSE)
  keeper_idx <- which(names(col_types) %in% keeper_cols)
  cc_arg <- ifelse(seq_along(col_types) %in% keeper_idx, NA, "NULL")
  read.csv(fname, stringsAsFactors=FALSE, colClasses=cc_arg, ...)
}
# 
read.csv_many <- function(folder, ...){
  folder %>% dir(pattern="\\.csv$", full.names=TRUE) %>% 
    lapply(read.csv, stringsAsFactors=FALSE, ...) %>% 
    (function(dfs) do.call("rbind", dfs)) 
}
read_selected_cols_many <- function(folder, keeper_cols, ...){
  folder %>% dir(pattern="\\.csv$", full.names=TRUE) %>% 
    lapply(read_selected_cols, keeper_cols=keeper_cols, ...) %>% 
    (function(dfs) do.call("rbind", dfs)) 
}
# 
################################### 



if (big_file){
  fname <- paste0(wrk, "childrens/workspace/in/data/follower_match_2.csv")
  keepers <- c("user_id", "user_name", "user_description", "text") 
} else {
  loc <- paste0(wrk, "UPenn/Twitter/ClassificationResults/Tobacco/")
  keepers <- c("tweet_id", "lang", "predict")
}






### NOW START THE RUNS [ON EITHER FOLDER CASE OR BIG FILE CASE]
sizes <- c(seq(from=100, to=switchpoint, by=100),            # for zoomed area 
           seq(from=switchpoint, to=upper_end, length.out=20))  # for main demo 


plot_outname <- ifelse(big_file, 
                       paste0("plot-big-file-", max(sizes), "rows.pdf"), 
                       paste0("plot-many-files-", max(sizes), "rows.pdf"))


# initialize container to catch results 
container <- dplyr::data_frame(
  size=sizes, 
  read.csv=rep(NA, length(sizes)), 
  read_selected_cols=rep(NA, length(sizes))
)
# try each size w both strats 
for (idx in seq_along(sizes)){
  
  if (big_file){
    ### large file case 
    container$read.csv[idx] <- round(system.time(
      d1 <- read.csv(fname, as.is=TRUE, nrows=sizes[idx])[, keepers]
    )["elapsed"], 4)
    container$read_selected_cols[idx] <- round(system.time(
      d2 <- read_selected_cols(fname, keepers, nrows=sizes[idx])
    )["elapsed"], 4)
    message("done with ", sizes[idx])
    
  } else {
    
    ### folder of files case 
    container$read.csv[idx] <- round(system.time(
      d1 <- read.csv_many(loc, nrows=sizes[idx])[, keepers]
    )["elapsed"], 4)
    container$read_selected_cols[idx] <- round(system.time(
      d2 <- read_selected_cols_many(loc, keeper_cols=keepers, nrows=sizes[idx])
    )["elapsed"], 4)
    message("done with ", sizes[idx])
    
  }
  
}



container <- container %>% reshape2::melt(id.vars="size")

main_plot <- container %>% 
  # begin plot block 
  ggplot(aes(x=size, y=value, color=variable)) + 
  geom_line() + 
  scale_x_continuous(breaks=round(seq(from=0, to=max(sizes), 
                                      by=max(sizes)/10)), 
                     expand=c(.01,0), limits=c(0, max(sizes)+.02*max(sizes))) + 
  labs(x="number of rows", y="elapsed time (seconds)", 
       subtitle=the_tit) + 
  annotate("rect", xmin=0, xmax=switchpoint, ymin=0, ymax=3, 
           color="black", fill="transparent")

window_ymax <- max(container$value[container$size <= switchpoint]) * 1.5


zoom_plot <- container %>% filter(size <= switchpoint) %>%
  # begin plot block 
  ggplot(aes(x=size, y=value, color=variable)) + 
  geom_line() + 
  scale_x_continuous(breaks=seq(from=100, to=switchpoint, length.out=3), 
                     labels=seq(from=100, to=switchpoint, length.out=3), 
                     expand=c(.02,.1)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, window_ymax)) + 
  theme(legend.position="none", 
        panel.grid=element_blank(), 
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=rel(.75)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        plot.margin = unit(c(1,1,1,1),"mm"))


maxtime <- max(container$value)

zoom <- ggplotGrob(zoom_plot)
main_plot + annotation_custom(grob=zoom, 
                              xmin=1000, xmax=max(sizes)/3.5, 
                              ymin=maxtime*1/3, ymax=maxtime*3/4)

ggsave(plot_outname, width=7, height=4, units="in", scale=1.2)


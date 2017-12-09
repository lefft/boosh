# TODO: 
#   - rep((1:6)[c(F,T)]) 
#   - .leap.seconds[1:3]
#   - cols odd: arguments imply differing number of rows: 105, 112
lefftpack::lazy_setup(sparse_grid=TRUE)
library("magick")
theme_set(theme_get() + theme(
  axis.line=element_blank(), 
  axis.title=element_blank(), 
  axis.ticks=element_blank(),
  legend.position="none"
))

# define a plinko board 
plinko_board <- function(rows, cols){
  odd  <- seq(from=1, to=cols, by=2); even <- seq(from=2, to=cols, by=2)
  x <- rep(1:rows, each=cols/2) + .5
  y <- rep(unlist(rep(list(odd, even))), 
           length=rows*cols/2) 
  return(data.frame(x, y))
}
# random step across a plinko board 
plinko_step <- function(x, y){
  horiz <- sample(c(-1,1), size=1)
  vert <- 1 #sample(c(0,1), prob=c(.25, .75), size=1)
  return(c(x+horiz, y-vert))
}

# define a plinko run 
# board=plinko_board(rows=6, cols=6); start=5
play <- function(board, start){
  xpos <- start + .5
  ypos <- max(board$y) #+ 1
  xwalk <- c(xpos)
  ywalk <- c(ypos)
  loc <- c(xpos, ypos)
  while (loc[2] != 1){
    loc <- plinko_step(xpos, ypos)
    xpos <- ifelse(loc[1] > max(board$x), max(board$x) -1, 
                   ifelse(loc[1] < min(board$x), min(board$x) + 1, loc[1]))
    ypos <- loc[2]
    xwalk <- c(xwalk, xpos) 
    ywalk <- c(ywalk, ypos)
  }
  run <- data.frame(step=seq_along(xwalk), xwalk, ywalk) 
  print(plot_run(board, run))
  return(run)
}
# plot a plinko run 
plot_run <- function(board, run){
  ggplot(board, aes(x=x, y=y)) + 
    geom_point(shape=21, size=rel(7)) + 
    # geom_label(aes(x=x, y=y, label=paste0(x, ",", y))) + 
    geom_path(data=run,aes(x=xwalk, y=ywalk, color=step), 
              size=rel(2), alpha=.5) + 
    scale_color_continuous(low="lightgray", high="darkgreen") +
    geom_point(data=run, aes(x=xwalk, y=ywalk), size=rel(3), color="darkgreen") 
    # geom_text(data=run, aes(x=xwalk, y=ywalk, label=paste0(xwalk, ",", ywalk)))
}

### ---------------------------------------------------------------------------

# NEED TO FIX COLS/ROWS ARGUMENTS!! 
df <- plinko_board(rows=20, cols=20) %>% play(start=15.5)

for (x in seq(from=2, to=nrow(df), by=1)){
  
  myplot <- plot_run(board=plinko_board(rows=20, cols=20), run=df[1:x, ])
  
  fname_digit <- ifelse(x < 10, paste0("0", x), as.character(x))
  
  ggsave(
    filename=paste0("plots/myplot", fname_digit, ".png"), plot=myplot, 
    width=7, height=5, units="in"
  )
}
gifcap <- c("yayyy, animated gif plot! \nwoop woop ~~ <(o_@)^")
dir("plots/")[endsWith(dir("plots/"), ".png")] %>% 
  (function(x) paste0("plots/", x)) %>% 
  image_read() %>% image_scale("x600") %>% image_join() %>%
  image_animate(fps=2) %>% 
  image_annotate("", location="+100+100", color="green", size=30) %>% 
  image_write(path="animate_woop_woop.gif", quality=10)


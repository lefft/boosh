lefftpack::lazy_setup(sparse_grid=TRUE) 







# going to vary these for 8 runs: 
#   - _1small_2big / _1big_2small  
#   - -2descending / -2ascending 
#   - vary_sh1_in_sh2 <- TRUE / vary_sh1_in_sh2 <- FALSE 
for (run_idx in 1:8){

# swap these vals every second run (after 2, 4, 6)
shape1s <- ((0:20)*1) 

shape2s <- c(1, seq(from=5, to=20, by=5))

if (run_idx %in% 5:8){
  shape1s <- c(1, seq(from=5, to=20, by=5))
  shape2s <- ((0:20)*1) 
}

if (run_idx %in% c(3,4,7,8)) shape2s <- rev(shape2s)

suffix <- rep(c(
  "_1small_2big-2ascending", "_1small_2big-2descending", 
  "_1big_2small-2ascending", "_1big_2small-2descending"), 
  each=2)[run_idx]
vary_sh1_in_sh2 <- ifelse(run_idx %% 2 == 0, FALSE, TRUE)



delete_pngs <- TRUE
n <- 10000; xlims <- c(0, 1); ylims <- c(0, 20)

folder <- paste0(ifelse(vary_sh1_in_sh2, 
                        "beta-plots_1in2", "beta-plots_2in1"),suffix)
if (!dir.exists(folder)){ dir.create(folder) }

frames_per_sec <- 10
giffie_outname <- paste0(folder, ".gif")

annnepos <- c(x=max(xlims), y=max(ylims))


pshap <- function(shp) 
  ifelse(shp %% 1 == 0, paste0(shp, ".0"), ac(shp))

for (sh1_idx in seq_along(shape1s)){
  shape1 <- shape1s[sh1_idx]
  
  for (sh2_idx in seq_along(shape2s)){
    
    shape2 <- shape2s[sh2_idx]
    
    # TO HAVE n VARY WITHIN VAR 
    if (vary_sh1_in_sh2){
      pname <- paste0(folder, "/plot", "-idx-", 
                      sprintf("%02d", sh2_idx), "-",
                      sprintf("%02d", sh1_idx), "-",
                      "_sh2", sprintf("%04s", pshap(shape2)),
                      "_sh1", sprintf("%04s", pshap(shape1)),".png")
    } else {
      pname <- paste0(folder, "/plot", "-idx-", 
                      sprintf("%02d", sh1_idx), "-",
                      sprintf("%02d", sh2_idx), "-",
                      "_sh1", sprintf("%04s", pshap(shape1)),
                      "_sh2", sprintf("%04s", pshap(shape2)),".png")
    }
    # NOTE: documentation for *beta() is great, cheeck out again 
    samps <- rbeta(n, shape1=shape1, shape2=shape2)
    
    annne <- paste0("n = ", sprintf("%04s", n), 
                    "\nsh1 = ", sprintf("%04s", pshap(shape1)), 
                    "\nsh2 = ", sprintf("%04s", pshap(shape2)))
    
    pp <- data.frame(beta_samp=samps) %>% 
      ggplot(aes(x=beta_samp)) + 
      geom_density(alpha=.5, fill="darkgreen", show.legend=FALSE) + 
      geom_vline(aes(xintercept=mean(beta_samp)), 
                 color="darkgreen", alpha=.25) + 
      labs(x="", y="") + 
      scale_x_continuous(limits=xlims, expand=c(0,0), 
                         breaks=c(0, .25, .5, .75, 1), 
                         labels=c("0",".25",".5",".75","1")) + 
      # scale_y_continuous(limits=ylims, expand=c(0,0)) +
      coord_cartesian(xlim=xlims, ylim=ylims, expand=FALSE) + 
      annotate("label", x=annnepos["x"], y=annnepos["y"], 
               label=annne, vjust=1.1, hjust=1.1) 
    
    ggsave(filename=pname, plot=pp, 
           width=5, height=3.5, units="in")#, scale=2)
    message("done with sh1 = ", pshap(shape1), 
            " sh2 = ", pshap(shape2))
            
  }
}


library("magick")

dir(folder, pattern=".png$", full.names=TRUE) %>% 
  image_read() %>% 
  image_scale("x600") %>% 
  image_join() %>%
  image_animate(fps=frames_per_sec) %>% 
  image_write(path=giffie_outname, quality=2)

if (delete_pngs){ 
  lapply(dir(folder, full.names=TRUE), unlink) 
  unlink(paste0(folder, "/"), recursive=TRUE)
}
message("done with run ", run_idx)
}


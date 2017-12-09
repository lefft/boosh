lefftpack::lazy_setup(sparse_grid=TRUE) 
library("magick")

shape1s <- ((0:40)*1) 
shape2s <- c(1, seq(from=5, to=25, by=5))


vary_sh1_in_sh2 <- TRUE 

delete_pngs <- TRUE
n <- 10000

xlims <- c(0, 1)
ylims <- c(0, 20)


folder <- paste0(ifelse(vary_sh1_in_sh2, 
                        "beta-plots_1in2", "beta-plots_2in1"))
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
      coord_cartesian(xlim=xlims, ylim=ylims, expand=FALSE) + 
      annotate("label", x=annnepos["x"], y=annnepos["y"], 
               label=annne, vjust=1.1, hjust=1.1) 
    
    ggsave(filename=pname, plot=pp, 
           width=5, height=3.5, units="in")
    
    message("done with sh1 = ", pshap(shape1), " sh2 = ", pshap(shape2))
      
    }
  }


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




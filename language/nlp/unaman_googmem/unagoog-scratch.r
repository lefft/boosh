### SCRATCH AND DEV AREAYAYA --------------------------------------------------
if (FALSE){
  bible <- get_bible()
  bgbib <- bible %>% 
    mutate(bigram = paste(w1, w2, sep=" ")) %>% 
    filter(!w1 %in% stopzzze) %>% filter(!w2 %in% stopzzze) %>% 
    group_by(bigram) %>% summarize(count = n()) %>% arrange(desc(count))
  
  doc_intersection(u, g, nm["u"], nm["g"], stoplist=stops)
  
  if (save_piqqz){
    ug_plot <- ug_intersect$bigram_plot
    outname <- paste0("out/unagoog-", nrow(ug_df), "shared-", slist, ".pdf")
    ggsave(plot=ug_plot, filename=outname, height=8, width=10, units="in")
  }
  
  # bgg <- text2bg_count(g) %>% mutate(source="googal")
  # bgu <- text2bg_count(u) %>% mutate(source="unaman")
}


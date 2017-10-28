library(tidyverse); library(forcats); library(ggjoy)
ce <- ggjoy::Catalan_elections

ce %>% 
  ggplot(aes(y = Year)) +
  geom_joy(aes(x = Percent, fill = paste(Year, Option)), 
           alpha = .8, color = "white", from = 0, to = 100) + 
  scale_fill_cyclical(values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff")) 

dat <- "../../Dropbox/NotVeryExperiment/Analyses-ComparativeAdj/sandbox/plotting_summarizing/out/data_clean_screened.csv" %>% read.csv(stringsAsFactors=FALSE)


dat %>% group_by(Pred2, NormUnit, Adj, Comparative) %>% 
  summarize(mean_resp=mean(response)) %>% ungroup() %>% 
  ggplot(aes(y=factor(NormUnit))) +
  geom_joy(aes(x = mean_resp, fill = Comparative), 
           alpha = .8, color = "white", from = 0, to = 100) + 
  scale_fill_cyclical(values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff")) #+
  # facet_grid(Adj ~ Comparative, scales="free_y")#


#####################



Catalan_elections %>%
  mutate(YearFct = fct_rev(as.factor(Year))) %>%
  ggplot(aes(y = YearFct)) +
  geom_joy(aes(x = Percent, fill = paste(YearFct, Option)), 
           alpha = .8, color = "white", from = 0, to = 100) +
  labs(x = "Vote (%)",
       y = "Election Year",
       title = "Indy vs Unionist vote in Catalan elections",
       subtitle = "Analysis unit: municipalities (n = 949)",
       caption = "Marc Belzunces (@marcbeldata) | Source: Idescat") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_cyclical(breaks = c("1980 Indy", "1980 Unionist"),
                      labels = c(`1980 Indy` = "Indy", `1980 Unionist` = "Unionist"),
                      values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
                      name = "Option", guide = "legend") +
  theme_joy(grid = FALSE)




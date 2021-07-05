salem_c
View(salem_c)
library(extrafont)
loadfonts(device = "win")
library(kableExtra)
require(dplyr)
install.packages("kableExtra")



##TABLE_VIEW :-
'------------------------'
salem_c %>% group_by(PLACE) %>% summarise(n = max(n)) %>% arrange(desc(n)) %>%  top_n(20) %>%  mutate(scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("STATION", "CASES", "% of CASES"),
    align = "llrr"
  )


#PLOT_VIEW :-
'-------------------------'
salem_c %>% group_by(PLACE) %>% summarise(n = max(n)) %>% arrange(desc(n)) %>%  top_n(8) %>% ggplot(aes(reorder(PLACE,-n), n, fill = PLACE)) +
  geom_col(alpha = 0.8, position = "dodge", show.legend = FALSE,  width=0.4) + labs(x = NULL, y = "Case Numbers") + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),text = element_text( size = 12, family = "Open Sans")) + theme(axis.text.x=element_text(angle=45, hjust=1))


'-------------------'
'Extract Top N Highest Values by Group Using dplyr'



library("dplyr")
salem_add_c <- salem_c %>% arrange(desc(n)) %>% group_by(NEW_DATE) %>%
  filter(
    NEW_DATE != 2014
  )%>% slice(1:3) 




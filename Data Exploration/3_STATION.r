
#PACKAGES :-
'--------------------'
library(extrafont)
loadfonts(device = "win")
library(kableExtra)
require(dplyr)
library(ggplot2)

library(tidyverse)
library(lubridate)
library(RSocrata)

##TABLE_VIEW :-
'------------------------'

salem_c
salem_c %>% group_by(PLACE) %>%
  summarise(n = max(n)) %>%
  arrange(desc(n)) %>% 
  top_n(20) %>%
  mutate(scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("STATION", "CASES", "% of CASES"),
    align = "llrr"
  )


#PLOT_VIEW :-
'-------------------------'
salem_c %>% group_by(PLACE) %>%
  summarise(n = max(n)) %>%
  arrange(desc(n)) %>%  top_n(8) %>%
  ggplot(aes(reorder(PLACE,-n), n, fill = PLACE)) +
  geom_col(alpha = 0.8, position = "dodge", show.legend = FALSE,  width=0.4)
+ labs(x = NULL, y = "Case Numbers")
+ theme_minimal() 
+ theme(plot.title = element_text(hjust = 0.4),
        text = element_text( size = 12, family = "Open Sans"))
+ theme(axis.text.x=element_text(angle=45, hjust=1))


'----------------------------------------------'

'-------------------'
'Extract Top N Highest Values by Group Using dplyr'



library("dplyr")
salem_add_c <- salem_c %>% arrange(desc(n)) %>% group_by(NEW_DATE) %>%
  filter(
    NEW_DATE != 2014
  )%>% slice(1:3) 


salem_add
'ggplot2'

salem_add_c <- salem_c %>% arrange(desc(n)) %>% group_by(NEW_DATE) %>%
  filter(
    NEW_DATE != 2014
  ) %>% slice(1:5) %>% ggplot(aes(x=NEW_DATE, y=n, fill=reorder(PLACE,-n), label = scales::percent(n))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + labs(x = NULL, y = "Case Numbers") + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),
        text = element_text( size = 12, family = "Open Sans")) + theme(axis.text.x=element_text(angle=45, hjust=1))+ guides(fill=guide_legend(title="Places")) 

salem_add_c
'---------------------------------------------------------------'
#More meaningful graph

salem_n <- salem_c %>% arrange(desc(n)) %>% group_by(NEW_DATE) %>%
  filter(
    NEW_DATE != 2014
  ) %>% slice(1:5) %>% mutate(pct = prop.table(n))



salem_n <- salem_c %>% arrange(desc(n)) %>% group_by(NEW_DATE) %>%
  filter(
    NEW_DATE != 2014
  ) %>% slice(1:5) %>% mutate(pct = prop.table(n))%>% 
  ggplot(aes(x = NEW_DATE, y = n, fill = reorder(PLACE,-n), label = scales::percent(pct))) + 
  geom_col(alpha = 0.8, position = "dodge", show.legend = TRUE,  width=0.80, colour="black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.4,    # nudge above top of bar
            size = 3)  + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),
                              text = element_text( size = 12, family = "Open Sans")) + 
theme(axis.text.x=element_text(angle=45, hjust=1)) + guides(fill=guide_legend(title="Places"))+
labs(x = NULL, y = "Case Numbers")


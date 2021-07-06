test_data_three

#PACKAGES
library(extrafont)
loadfonts(device = "win")
library(kableExtra)
require(dplyr)
library(ggplot2)

library(tidyverse)
library(lubridate)
library(RSocrata)
library(readxl)


test_data_three




salem_trak_change <- test_data_three %>% mutate() %>% mutate(time = as.numeric(time))
str(salem_trak_change)
salem_trak_change_new <- salem_trak_change %>% mutate() %>% mutate(time = as.numeric(time)) %>%
  mutate(
    # Create categories
    time_period = dplyr::case_when(
      time >= 7 & time <= 10           ~ "MOH",
      time >= 10 & time <= 16 ~ "AN",
      time >= 16 & time <= 21 ~ "EOH",
       time >= 21             ~ "N",
      time < 7  ~ "N"
    ),
    # Convert to factor
    time_period = factor(
      time_period,
      level = c("MOH", "AN","EOH", "N")
    )
  )

salem_trak_change_new

salem_term <- salem_trak_change_new %>%  filter(
  GENDER != "N"
)  %>% count(YEAR, time_period)


salem_term %>% 
  mutate(scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("YEAR", "time_period", "Number of CASES", "% of CASES"),
    align = "llrr"
  )

salem_term %>% 
  mutate(scales::percent(n / sum(n)))

'---------------------------------------------------------------'

salem_add_c <- salem_term %>% arrange(desc(n)) %>%  ggplot(aes(x=YEAR, y=n, fill=reorder(time_period,-n), label = scales::percent(n))) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + labs(x = NULL, y = "Case Numbers") + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),
                                                                                                                                      text = element_text( size = 12, family = "Open Sans")) + theme(axis.text.x=element_text(angle=45, hjust=0.5))+ guides(fill=guide_legend(title="Places")) 

salem_add_c


'-----------------------------------------'
#More Meaningful graph

salem_n <- salem_term %>% arrange(desc(n)) %>% group_by(YEAR) %>% mutate(pct = prop.table(n))

salem_n


salem_n$YEAR <- factor(salem_n$YEAR)

salem_n

salem_no <- salem_n %>% mutate(time = as.factor(YEAR)) %>% arrange(desc(n)) %>% group_by(YEAR) %>% mutate(pct = prop.table(n))%>% 
  ggplot(aes(x = YEAR, y = n, fill = reorder(time_period,-n), label = scales::percent(pct))) + 
  geom_col(alpha = 0.8, position = "dodge", show.legend = TRUE,  width=0.80, colour="black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.4,    # nudge above top of bar
            size = 3)  + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),
                                                 text = element_text( size = 12, family = "Open Sans")) + 
  theme(axis.text.x=element_text(angle=45, hjust=0.5)) + guides(fill=guide_legend(title="Time Period"))+
  labs(x = NULL, y = "Case Numbers") 

salem_no




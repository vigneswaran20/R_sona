library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)
salem_accidents <- excel_sheets("E:\\CopyOFnew.xlsx") %>% map_df(~read_xlsx("E:\\CopyOFnew.xlsx",.))
skimr::skim(salem_accidents)
str(salem_accidents)


'----------------------------------------------------------------------------------------------'
library(tidyverse)
library(lubridate)
library(RSocrata)

salem_acc_report <- salem_accidents
str(salem_acc_report)
salem_acc_report
skimr::skim(salem_acc_report)


##ignore the missing data

salem_acc <- salem_acc_report %>%
  filter( !is.na(GENDER), !is.na(CASES), !is.na(NEW_DATE))  %>%
  select(AGE, GENDER,CASES, NEW_DATE) %>% 
  mutate_if(is.character, factor)
 

salem_acc
skimr::skim(salem_acc)


'------------------------------------------------------------------------'

'How have the number of accidents changed over time? (in weeks)'

salem_trak = salem_acc %>% arrange(desc(NEW_DATE)) %>% na.omit()
salem_trak




salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)









salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  )  %>%
  ggplot(aes(NEW_DATE, n, color = CASES)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = NULL, y = "Number of traffic accidents per week",
    color = "CASES"
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

'-----------------------------------------------------------------------------------'
'How have the number of accidents changed over time? (in months)'
salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  )  %>%
  ggplot(aes(NEW_DATE, n, color = CASES)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = NULL, y = "Number of traffic accidents per month",
    color = "CASES"
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))


'--------------------------------------------------------------------------------------------------'
'How has the injury rate changed over time?'

salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  ) %>%
  group_by(NEW_DATE) %>%
  mutate(percent_injury = n / sum(n)) %>%
  ungroup() %>%
  filter(CASES == "Fatal") %>%
  ggplot(aes(NEW_DATE, percent_injury)) +
  geom_line(size = 1.5, alpha = 0.7, color = "midnightblue") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format()) +
  labs(x = NULL, y = "% of accidents that involve death") + 
theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

library(ggplot2)

library(tidyverse)
install.packages("percent_format")
str(CopyOFnew2)

tail(salem_trak)


'----------------------------------------------------------------'



salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES,  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  
  count(NEW_DATE, CASES)  %>%
  filter(CASES == "Fatal") %>%
  group_by(CASES) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("Day", "Cases", "Number of people", "percentage"),
    align = "llrr"
  )

'-----------------------------------------------------------------------'


'How does the cases rate change through the week?'


salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                         "Medium injury`"),  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  count(NEW_DATE, CASES) %>%
  group_by(CASES) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(percent, NEW_DATE, fill = CASES)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Cases", y = NULL, fill = "Cases") + ggtitle("Case Rate") +
  
  scale_fill_brewer(palette="Paired") +
 
 theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

'--------------------------------------------------------------------'
'How does the fatal rate change through the week?'

salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                                "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                                "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                                "Medium injury`"),  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  count(NEW_DATE, CASES) %>%
  group_by(CASES) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  filter(CASES == "Fatal") %>%
  ggplot(aes(percent, NEW_DATE, fill = CASES)) +
  geom_col(position = "dodge", alpha = 0.8, fill="red") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Accidents", y = NULL, fill = "Cases") + ggtitle("Fatality Rate") +
  
  ##scale_fill_brewer(palette="Paired") +
  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

'--------------------------------------------------------------------'
'How does the accident rate change through the week?'

library(extrafont)
loadfonts(device = "win")



salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Accident = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                                  "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                                  "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                                  "Medium injury`","Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  
  count(NEW_DATE, CASES)  %>%
  group_by(CASES) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("Day", "Cases", "Number of people", "percentage"),
    align = "llrr"
  )


library("knitr")





salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Accident = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                                  "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                                  "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                                  "Medium injury`","Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  count(NEW_DATE, CASES) %>%
  group_by(CASES) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(percent, NEW_DATE, fill = CASES)) +
  geom_col(position = "dodge", alpha = 0.8, fill="steelblue") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Accidents", y = NULL, fill = "Cases") + ggtitle("Accident Rate") +
  
  ##scale_fill_brewer(palette="Paired") +
  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))


'-----------------------------------------------------------------------------------------------'
#Gender Gap ? 


salem_manipulated <- salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Accident = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                                  "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                                  "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                                  "Medium injury`","Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%  filter(
    GENDER != "N"
  )  %>% count(GENDER)
salem_manipulated

ggplot(salem_manipulated, aes(x=GENDER, y=n)) + 
  geom_bar(stat = "identity", width=0.2, alpha = 0.4, fill="red") + theme_minimal() + labs(x = "GENDER", y = "NUMBER OF CASES") + ggtitle("GENDER GAP") + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

'------------------------------------------------------------------------------'
#Injuries vs Fatality?


salem_manipulated <- salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                                "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                                "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                                "Medium injury`"),  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%  count(CASES)
salem_manipulated

ggplot(salem_manipulated, aes(x=CASES, y=n)) + 
  geom_bar(stat = "identity", width=0.2, alpha = 0.4, fill="red") + theme_minimal() + labs(x = "CASE TYPES", y = "NUMBER OF CASES") + ggtitle("FATAL vs INJURY") + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))
'---------------------------------------------------------------------------'







#Gender with the highest fatality?

#Age group with highest accidents (injuries + fatality)?

#How have the number of accidents changed over seasons? (Spring , Autumn , Summer, Winter)

#Which place records highest accidents , fatality , injuries? (Only the station info is given you have to find the location of the station and record in the place column)

#How have the accidents with respect to places changes over years?

#The station that filed more accident?

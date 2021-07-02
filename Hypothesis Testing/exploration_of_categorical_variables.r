'Exploring categorical variables with Fischers and chi-square test'

library("tidyverse")
library("DataExplorer")


library(readxl)
library(lubridate)
library(RSocrata)
salem_accidents <- excel_sheets("E:\\CopyOFnew.xlsx") %>% map_df(~read_xlsx("E:\\CopyOFnew.xlsx",.))
skimr::skim(salem_accidents)
str(salem_accidents)


'----------------------------------------------------------------------------------------------'

salem_acc_report <- salem_accidents
str(salem_acc_report)
salem_acc_report

diamonds
skimr::skim(salem_acc_report)

##ignore the missing data

salem_acc <- salem_acc_report %>%
  filter( !is.na(GENDER), !is.na(CASES), !is.na(NEW_DATE))  %>%
  select(AGE, GENDER,CASES, NEW_DATE) %>% 
  mutate_if(is.character, factor)


salem_trak = salem_acc %>% arrange(desc(NEW_DATE)) %>% na.omit()
salem_trak


##Categorical variable exploration

salem_formatted <-  salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  filter(
    GENDER != "N"
  ) %>% 
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) 

salem_formatted %>%  filter(
  GENDER != "N"
) %>% count(CASES) 

salem_formatted %>%  filter(
  GENDER != "N"
) %>% count(GENDER) 

skimr::skim(salem_formatted)
plot_bar(salem_formatted, ggtheme = theme_minimal(base_size = 20))

'------------------------------------------------------------------------------'
##PERCENTAGE COMPARISON

library(SmartEDA)
library(ISLR)

salem_formatted

ExpCatViz(salem_formatted %>% select(CASES, GENDER), target = "CASES", col=c("slateblue4","slateblue1"))

'------------------------------------------------------------------------'
##FINDING THE MEDIAN AGE

plot4 <- ExpNumViz(salem_formatted,target="CASES",col=c("darkgreen","springgreen3","springgreen1"))
plot4[[1]]

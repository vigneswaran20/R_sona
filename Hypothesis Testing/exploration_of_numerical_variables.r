#Yet to do
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




install.packages("dlookr")
install.packages("flextable")
library(flextable)
library(dlookr)


dlookr::describe(salem_trak) %>% flextable()

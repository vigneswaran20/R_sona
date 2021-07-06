library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)

library(tidyverse)
library(lubridate)
library(RSocrata)
salem_accidents <- excel_sheets("E:\\CopyOFnew.xlsx") %>% map_df(~read_xlsx("E:\\CopyOFnew.xlsx",.))
skimr::skim(EDITED_COPY)
str(salem_accidents)


'----------------------------------------------------------------------------------------------'


salem_acc_report <- EDITED_COPY
str(salem_acc_report)
salem_acc_report
skimr::skim(salem_acc_report)


##ignore the missing data

salem_acc <- salem_acc_report %>%
  filter( !is.na(GENDER), !is.na(CASES), !is.na(NEW_DATE), !is.na(DATE))  %>%
  select(AGE, GENDER,CASES, NEW_DATE,DATE) %>% 
  mutate_if(is.character, factor)


df <- salem_acc 

df
library(tidyr)

test_data <- df %>% separate(DATE, into = c('time', 'month'), sep = 2) %>% select(-month)
View(test_data)

library("readr")

skimr::skim(salem_acc)
write.csv(test_data,"E:\\test_data_two.csv")

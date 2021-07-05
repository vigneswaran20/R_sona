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
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`")))
                                         
                                         
                                         


salem_trak_change <- salem_trak %>% mutate() %>% mutate(AGE = as.numeric(AGE))
str(salem_trak_change)
salem_trak_change %>% mutate() %>% mutate(AGE = as.numeric(AGE)) %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      AGE <= 14            ~ "0-14",
      AGE > 14 & AGE <= 44 ~ "15-44",
      AGE > 44 & AGE <= 64 ~ "45-64",
      AGE > 64             ~ "> 64"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-14", "15-44","45-64", "> 64")
    )
  )

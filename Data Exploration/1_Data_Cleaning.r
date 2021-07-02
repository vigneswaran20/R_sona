#Load the necessary libraries -------------------------------------------------->  step - 1

library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)



#Stitch multiple sheets into a single sheet. Note: Make sure all the sheets have the same variable name -------------------------------> step - 2

salem_accidents <- excel_sheets("E:\\CopyOFnew.xlsx") %>% map_df(~read_xlsx("E:\\CopyOFnew.xlsx",.))
salem_accidents
str(salem_accidents)




#Checking for missing data and data types {integer,character, factor, DATE, etc....} using skimr::skim() function. ---------------> step-3

salem_acc_report <- salem_accidents   
str(salem_acc_report)
skimr::skim(salem_acc_report)


#Ignoring the missing data. Remember that every data is important for this project, for consistency I am adding this extra step  ---------------------> step-4

salem_acc <- salem_acc_report %>%
  filter( !is.na(GENDER), !is.na(CASES), !is.na(NEW_DATE))  %>%
  select(AGE, GENDER,CASES, NEW_DATE) %>% 
  mutate_if(is.character, factor)
 
 
 
# Now you can check the cleaned , processed data using skimr::skim() function. -------> step -5





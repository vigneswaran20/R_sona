'Exploring numerical variables'

library("tidyverse")
library("DataExplorer")


library(readxl)
library(lubridate)
library(RSocrata)
salem_accidents <- excel_sheets("E:\\CopyOFnew.xlsx") %>% map_df(~read_xlsx("E:\\CopyOFnew.xlsx",.))
skimr::skim(salem_accidents)
str(salem_accidents)


'----------------------------------------------------------------------------------------------'

salem_accidents %>% diagnose_numeric() %>% flextable()


'--------------------------------------------------------'





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


'-----------------------------------------------'
salem_formatted <-  salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  filter(
    GENDER != "N"
  ) %>% 
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) 


'------------------------------------------------'

install.packages("dlookr")
install.packages("flextable")
library(flextable)
library(dlookr)
'-------------------------------------------------'
'dlookr'

dlookr::describe(salem_formatted) %>% flextable()

'-----------------------------------------------------'
'With tidyverse packages'

salem_formatted %>% diagnose_numeric() %>% flextable()

salem_formatted %>% group_by(CASES) %>% univar_numeric()

'--------------------------------------------------------'
library(SmartEDA)

ExpNumStat(salem_formatted, by="G",gp="CASES", round = 2) %>% flextable()

ExpNumStat(salem_formatted, by="GA", gp="CASES", Outlier = TRUE, Qnt = c(.25,.75), round = 2) %>% flextable()


'--------------------------------------------------------------'

library(summarytools)
install.packages("summarytool")
dfSummary(salem_formatted)
'--------------------------------------------------------'
'For checking categorical variable'

library(gtsummary)
install.packages("gtsummary")

salem_formatted %>% select(CASES, GENDER) %>% tbl_summary(by = CASES) %>% add_p() 
mtcars %>% select(cyl, gear, am) %>% tbl_summary(by = am)  %>% add_p() 
'For Numeric variables it uses Non-parametric Wilcoxon test for comparing with two groups'
'categorical variables are used in Fischers exact test if number of variables in any observations 
is below 5 or we use CHI-SQUARE test'
str(mtcars)
'-------------------------------------------------------------------------------'

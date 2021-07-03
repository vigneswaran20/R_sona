'Exploring numerical variables'

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

'Exploring categorical variables with Fischers and chi-square test'


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
'-------------------------------------------------------'

library(ggstatsplot)
ggbarstats(data = salem_formatted, x = CASES, y = GENDER, label = "both")

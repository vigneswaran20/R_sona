#Yet to do
'Explore Normality with QQ-PLOTS & SHAPIRO-WILK'


'If the data is normally distributed we can use parametric test like T-TEST, ANOVA'
'However if not normally ditributed we should use non-parametric test like Mann-Whitney or Kruskal-Wallis'
'ANOVA and Kruskal-Wallis for more than two groups'

## {DataExplorer}
library(DataExplorer)


plot_qq(salem_formatted)

plot_qq(salem_formatted, by = "CASES")
plot_qq(salem_formatted, by = "GENDER")
'----------------------------------------------------'
library(dlookr)

data("iris")

salem_formatted %>% group_by(CASES) %>% plot_normality(AGE)
'How close is close enough'

install.packages("ggpubr")

library(ggpubr)
ggqqplot(salem_formatted,"AGE", facet.by = "CASES")
ggqqplot(salem_formatted,"AGE", facet.by = "GENDER")


normality(salem_formatted) %>% mutate_if(is.numeric,~round(.,3)) %>% flextable::flextable()

'normality test with every numeric variable in the dataset'

salem_formatted %>% group_by(CASES,GENDER) %>% normality()

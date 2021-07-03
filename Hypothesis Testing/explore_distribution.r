'Why do we need to explore the distribution :-

Well, many statistical test actually depend on symmetric and normally distributed data.

So histograms and density plots, allow us to have first glimpse on the data for example

data explorer package provides very intuitive functions for getting histograms and density plots for

all continuous variables at once namely plot histogram and plot density'


'Exploring symmetricity'

'-----------------------------------------------------------------------'

## {DataExplorer}
library(DataExplorer)

plot_histogram(salem_formatted)
plot_density(salem_formatted)


#Works perfectly with dplyr package

salem_formatted %>% select(AGE) %>% plot_density()

'Now we can say that this one is kinda symmetric'

'Now how to check the symmetry of data and when is data symmetric enough,

They can be checked by two methods they are skewness and kurtosis'

library(moments)

skewness(salem_formatted$AGE, na.rm = TRUE)
'Here value is 0.1819392'

'If +1 it is far away from the zero'


'How far is far enough?'

agostino.test(salem_formatted$AGE)
'It gives p-value and we can check with that'

'Kurtosis is the measure of heavy tails or outliers present in the distribution'

#Kurtosis
anscombe.test(salem_formatted$AGE)
'based on the kurt value we can say whether the distribution is fine or not'

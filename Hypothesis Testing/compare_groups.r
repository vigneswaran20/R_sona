##{DataExplorer}

plot_boxplot(salem_formatted, by = "GENDER")
plot_boxplot(salem_formatted, by = "CASES")


## {ggstatsplot}

install.packages("ggstatsplot")
library(ggstatsplot)

ggbetweenstats(data = salem_formatted,x=CASES,y=AGE)

'If the variances of the two groups are similar then we will use student t-test instead of Welch
t test. Note by default it assumes the variances of these groups are different and goes for
Welch t test.

To answer this question we use levene test to check whether the variance differ significantly'

library(car)

leveneTest(data = salem_formatted, AGE~CASES)

'Here p-value is less than 0.05 so reject the null hypothesis and there is significance
between the  groups and tells us that the variances are not similar so our test is correct'

'If the p-value is greater than 0.05 then we can do the following'
ggbetweenstats(
  data = salem_formatted,x=CASES, y=AGE, var.equal = TRUE)

ggbetweenstats(
  data = salem_formatted,x=GENDER, y=AGE, var.equal = TRUE)

'The data is normally distributed thats why we see a parametric test oin the first place'
'Through the shape of the violin we can say the normality'

'We have to check the normality of the data inside of each group inorder to find the 
correct test. The shapiro Wilk normality test helps us with that'

shapiro.test(salem_formatted$AGE[salem_formatted$CASES =="Injury"])
shapiro.test(salem_formatted$AGE[salem_formatted$GENDER =="F"])

'Here the data is not normally distributed'
ggbetweenstats(
  data = salem_formatted,
  x = CASES,
  y = AGE,
  var.equal = FALSE,
  type = "np"
)
'If more than two groups then we can say ANOVA or KRUSKAL-WALLIS'

ggbetweenstats(
  data = salem_formatted,
  x = CASES,
  y = AGE,
  var.equal = FALSE,
  type = "np",
  p.adjust.method = "fdr",
  pairwise.display = "s"
)


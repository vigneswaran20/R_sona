
### {dlookr} - correlation

correlation(airquality, Ozone)


plot_correlate(airquality, method = "kendall")
ggcorrmat( data = iris)
ggcorrmat( data = iris, type = "np" , output = "dataframe" ) %>% mutate_if(is.numeric, ~round(.,2)) %>% flextable()

ggscatterstats(
data = airquality,
  x = Ozone,
  y = Temp,
  type = "np"
)


chart.Correlation(iris %>% select(-Species), method= "kendall")


library(fastStat)


iris %>% select_if(is.numeric) %>% cor_sig_star(method = "kendall")



#dlookr

bla <- compare_numeric(iris)

bla$correlation

bla$linear %>% mutate_if(is.numeric, ~round(.,2)) %>% flextable()

plot(bla)


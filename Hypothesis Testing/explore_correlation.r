
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

library(ggplot2)
ggplot(iris, aes(Sepal.length, Sepal.Width))+
geom_point() +
geom_smooth() +
facet_wrap(~Species)

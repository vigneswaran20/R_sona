salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  )  %>%
  ggplot(aes(NEW_DATE, n, color = CASES)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = NULL, y = "Number of traffic accidents per month",
    color = "CASES"
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

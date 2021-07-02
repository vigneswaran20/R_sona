
#How have the number of accidents changed over time? (in weeks) ------------------------------------------->  1

salem_trak = salem_acc %>% arrange(desc(NEW_DATE)) %>% na.omit()


salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  )  %>%
  ggplot(aes(NEW_DATE, n, color = CASES)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = NULL, y = "Number of traffic accidents per week",
    color = "CASES"
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #How have the number of accidents changed over time? (in months)
salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
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
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))
  
  ##----------------------------------------------------------------------------------------------------------------------------------------------------
  #How has the death rate changed over time? (in months)
  
  salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  ) %>%
  group_by(NEW_DATE) %>%
  mutate(percent_injury = n / sum(n)) %>%
  ungroup() %>%
  filter(CASES == "Fatal") %>%
  ggplot(aes(NEW_DATE, percent_injury)) +
  geom_line(size = 1.5, alpha = 0.7, color = "midnightblue") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format()) +
  labs(x = NULL, y = "% of accidents that involve death") + 
theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

##------------------------------------------------------------------------------------------------------------------------------------------------------

#The day in the week with the highest falaity rate/ injury rate?

salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                         "Medium injury`"),  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  count(NEW_DATE, CASES) %>%
  group_by(CASES) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  filter(CASES == "Injury") %>%
  ggplot(aes(percent, NEW_DATE, fill = CASES)) +
  geom_col(position = "dodge", alpha = 0.8, fill="steelblue") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Accidents", y = NULL, fill = "Cases") + ggtitle("Fatality Rate") +
  
  ##scale_fill_brewer(palette="Paired") +
 
 theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

##-------------------------------------------------------------------------------------------------------------------------------------------------
#The day in the week with the highest falaity rate/ injury rate? (Finished)
 #How has the death rate changed over time? (in months) (Finished)
 #How has the accident rate changed over time?   (Finished)
#The day in the week with the highest falaity rate/ injury rate? (Finished)
#How have the number of accidents changed over time? (in weeks) (in months)  (Finished)

#Gender with the highest fatality rate ? 

#Gender with the highest accidents (injuries + fatality)?

#Age group with highest accidents (injuries + fatality)?

#How have the number of accidents changed over seasons? (Spring , Autumn , Summer, Winter)

#Which place records highest accidents , fatality , injuries? (Only the station info is given you have to find the location of the station and record in the place column)

#How have the accidents with repsect to places changes over years?

#The station that filed more accident?



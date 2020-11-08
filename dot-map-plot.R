umrti_base %>% 
  ggplot(aes(x = floor_date(datum, "week", week_start = 1), y = vek)) +
  geom_tile(fill = "white", colour = "grey80") +
  geom_jitter(alpha = .1, size = .6, colour = "firebrick") +
  scale_x_date(date_breaks = "month", 
               labels = label_date_short(), 
               expand = expansion(0, 0)) +
  ptrr::theme_ptrr("none", axis.text.x = element_text(hjust = 1))

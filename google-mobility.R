source("google_load-transform.R")

gd_dlouhe %>% 
  filter(kraj_nazev == "Celá ČR",
         date > "2020-09-01") %>% 
  ggplot(aes(date, zmena)) +
  geom_line(size = .5, colour = "grey") +
  geom_smooth(aes(colour = kategorie_cz), size = .5, show.legend = F) +
  facet_wrap(~ kategorie_cz) +
  theme_ptrr("both", multiplot = T) +
  labs(title = "Mobilita podle typu místa",
       subtitle = str_glue("100 = před-Covid průměr pro daný den v týdnu\n",
                           "šedá čára = data, barevná = Loess vyhlazení"),
       caption = "@ petrbouchal z dat Google mobility")

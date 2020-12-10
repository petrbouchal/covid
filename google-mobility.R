source("google_load-transform.R")

gd_dlouhe %>% 
  filter(kraj_nazev == "Celá ČR") %>% 
  ggplot(aes(date, zmena)) +
  geom_line(aes(colour = kategorie_cz), size = .5) +
  facet_wrap(~ kategorie)

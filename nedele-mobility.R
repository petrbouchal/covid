library(tidyverse)
library(skimr)
library(ptrr) # remotes::install_github("petrbouchal/ptrr")
library(ggrepel)
library(lubridate)

source("google_load-transform.R")

ptrr::set_ptrr_ggplot_fonts()
ptrr::set_geom_defaults()

vikend_obchody <- gd_dlouhe %>% 
  filter(kraj_nazev == "Celá ČR",
         date > "2020-10-15",
         kategorie %in% c("retail_and_recreation", "grocery_and_pharmacy")) %>% 
  mutate(denvtydnu = wday(date, label = T, abbr = T, week_start = 1) %>% 
           fct_relevel("Mon", "po", after = Inf),
         denvtydnu_num = wday(date, week_start = 1),
         tyden_od = floor_date(date, "week", week_start = 2) %>% factor(ordered = T),
         tyden_label = floor_date(date, "week", week_start = 2) %>% format("%d. %b")) %>% 
  filter(denvtydnu_num %in% c(1, 5:7)) %>%
  ungroup() %>% 
  arrange(tyden_od)

sndys <- vikend_obchody %>% 
  filter(denvtydnu_num == 7) %>% 
  select(tyden_od, denvtydnu, zmena, tyden_label, kategorie_cz, date)

ggplot(vikend_obchody, aes(denvtydnu, y = zmena)) +
  geom_point(aes(colour = tyden_od, group = tyden_od)) +
  geom_line(aes(colour = tyden_od, group = tyden_od)) +
  geom_point(data = sndys, aes(fill = tyden_od, group = tyden_od), 
             size = 3, colour = "white", shape = 21) +
  facet_wrap(~kategorie_cz) +
  theme_ptrr(family = "IBM Plex Sans Condensed", title_family = "IBM Plex Sans", multiplot = T, base_size = 16) +
  geom_label_repel(data = sndys, lineheight = .9, family = "IBM Plex Sans Condensed",
                   aes(label = format(date, "%d/%m"),
                       fill = tyden_od),
                   segment.colour = NA,
                   colour = "white", label.size = 0,
                   hjust = 1, nudge_y = 2, fontface = "bold") +
  scale_fill_viridis_d(option = "A", end = c(.9), direction = -1,
                       aesthetics = c("colour", "fill")) +
  guides(fill = "none", colour = "none") +
  labs(title = "Víkendy a zákaz nedělního prodeje",
       subtitle = "Míra aktivity: 100 = před-Covid normál daného dne",
       caption = "@petrbouchal z dat Google Mobility")

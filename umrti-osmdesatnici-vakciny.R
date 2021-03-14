library(tidyverse)
library(ptrr)
library(czso)
library(lubridate)
library(scales)
library(slider)

Sys.setlocale(locale = "cs_CZ.UTF-8", category = "LC_ALL")

ptrr::set_ptrr_ggplot_fonts()
ptrr::set_geom_defaults()

cisokr <- czso_get_codelist(109) %>% 
  select(okres_lau_kod = CHODNOTA, okres = ZKRTEXT)
ciskraj <- czso_get_codelist(108) %>% 
  select(kraj_nuts_kod = CHODNOTA, kraj = ZKRTEXT)

umrti <- read_csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/umrti.csv") %>% 
  left_join(cisokr) %>% 
  left_join(ciskraj) %>% 
  mutate(day_sameyear = make_date(2020, month = month(datum),
                                  day = day(datum)),
         day_sameyear_floored = floor_date(day_sameyear, "week", week_start = 1), 
         tyden = isoweek(datum),
         rok = year(datum),
         rok_weekbased = format(datum, "%G"),
         roktyden_iso = paste0(rok_weekbased, "-W", 
                               str_pad(tyden, width = 2, pad = 0)
         ))

um80 <- umrti %>% 
  mutate(nad80 = vek > 80,
         vek_label = ifelse(nad80, "mladší než\n80 let", "starší než\n80 let"),
         y_label = ifelse(nad80, 113, 38),
         tyden = isoweek(datum)) %>% 
  group_by(nad80, vek_label, datum) %>% 
  summarise(y_label = min(y_label), n = n(), .groups = "drop") %>%
  group_by(vek_label) %>% 
  mutate(prm = slide_dbl(n, mean, .after = 7)) %>% 
  group_by(datum) %>% 
  mutate(podil_prumeru = prm/sum(prm))

ggplot() +
  geom_line(data = um80, mapping = aes(datum, prm, colour = nad80), size = .5) +
  scale_colour_manual(values = c("grey30", "blue"), guide = NULL) +
  scale_fill_manual(values = c("grey30", "blue"), guide = NULL) +
  geom_vline(xintercept = as.Date("2021-01-14"), colour = "blue", linetype = "dotted") +
  geom_label(aes(x = datum, label = vek_label, y = y_label, fill = vek_label),
             colour = "white", 
             data = um80 %>% filter(datum == as.Date("2021-02-06"))) +
  annotate("rect", xmin = as.Date("2021-01-14"), xmax = Sys.Date(), colour = NA,
           ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1) +
  annotate("text", x = as.Date("2021-01-16"), y = 10, hjust = 0,
           label = "začátek registrace\nna očkování\npro 80+",
           colour = "blue") +
  scale_x_date(labels = scales::label_date_short(),
               breaks = breaks_width("1 month")) +
  labs(title = "Zemřelí na Covid 19: možný vliv očkování lidí 80+",
       subtitle = "denní počty zemřelých, sedmidenní klouzavý průměr",
       caption = "Zdroj: data MZd ČR\npetrbouchal.xyz/covid") +
  theme_ptrr("both")

um80 %>% 
  filter(nad80, datum > "2020-10-01") %>% 
  ggplot() +
  geom_vline(xintercept = as.Date("2021-01-14"), colour = "blue", linetype = "dotted") +
  annotate("rect", xmin = as.Date("2021-01-14"), xmax = Sys.Date(), colour = NA,
           ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1) +
  annotate("text", x = as.Date("2021-01-16"), y = .6, hjust = 0,
           label = "začátek registrace\nna očkování\npro 80+", colour = "blue") +
  scale_y_percent_cz(limits = c(0,.75)) +
  scale_x_date(labels = scales::label_date_short(),
               breaks = breaks_width("1 month")) +
  geom_line(aes(datum, podil_prumeru), size = .6) +
  labs(title = "Zemřelí na Covid 19: možný vliv očkování lidí 80+",
       subtitle = "podíl osob 80+ na denním počtu zemřelých, sedmidenní klouzavý průměr",
       caption = "Zdroj: data MZd ČR\npetrbouchal.xyz/covid") +
  theme_ptrr("both")

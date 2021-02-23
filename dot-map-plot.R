library(tidyverse)
library(czso)
library(lubridate)
library(scales)
library(ptrr)

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
         tyden = if_else(tyden == 53 & rok == 2021, 1, tyden),
         roktyden = paste0(rok, "-W", str_pad(tyden, width = 2, pad = 0)) %>%
           recode(`2021-W53` = "2021-W01"),
  )

umrti_base_dots <- umrti %>% 
  mutate(id = 1:n(),
         vek_range = cut_width(vek, width = 10, boundary = 0, ordered_result = T) %>% 
           fct_relabel(str_remove_all, "\\(|\\)|\\]|\\[") %>% 
           fct_relabel(str_replace, "\\,", " - "),
         pocet = 1) %>% 
  # group_by(vek_range) %>%
  mutate(selected = rbinom(n = n(), size = 1, prob = 7 * vek^-2)) %>% 
  replace_na(list(selected = 0)) %>% 
  mutate(label = str_glue("{if_else(pohlavi == 'M', 'muž', 'žena')}, {vek} let\n", 
                          "{okres}\n",
                          "{str_sub(format_date_human(datum), end = -6)}"),
         label = if_else(selected == 1, label, NA_character_))

mean(umrti_base_dots$selected)
sum(umrti_base_dots$selected)

celkem <- sum(umrti_base_dots$pocet, na.rm = T)

umrti_base_dots %>% 
  count(selected, wt = mean(vek))
  

umrti_base_dots %>%
  ungroup() %>% 
  arrange(selected) %>% 
  # sample_n(3000) %>%
  ggplot(aes(x = datum, y = vek_range)) +
  # geom_tile(fill = "black", colour = NA) +
  geom_point(aes(y = vek),
             alpha = 1, size = 1, colour = "white",
             position = position_jitter(seed = 1L, height = .6, width = .6)) +
  scale_x_date(date_breaks = "month", 
               labels = label_date_short(), 
               expand = expansion(0, 0)) +
  ptrr::theme_ptrr("none", axis.text.x = element_text(hjust = 0), 
                   axis.title.y = element_text(angle = 90, hjust = .93, vjust = 0,
                                               colour = "grey20"),
                   panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black"), 
                   text = element_text(colour = "white")) +
  labs(title = label_number_cz()(celkem), y = "věk") +
  gghighlight::gghighlight(selected == 1, label_key = label, use_direct_label = T, 
                           label_params = list(label.size = 0, fill = "grey10", size = 2, alpha = .75,
                                               segment.alpha = .4,
                                               family = "IBM Plex Sans",
                                               colour = "grey80", hjust = 0, lineheight = .8
                                               ),
                           unhighlighted_params = list(colour = "firebrick", size = .6, alpha = .3))

ggsave("dots-twitter.png", device = ragg::agg_png(width = 1500, height = 1500*9/16, res = 200, scaling = .6))
       
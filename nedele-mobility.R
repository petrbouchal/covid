library(tidyverse)
library(skimr)
library(ptrr) # remotes::install_github("petrbouchal/ptrr")
library(ggrepel)
library(lubridate)

tf <- tempfile(fileext = ".zip")

download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
              destfile = tf)

writeLines(
  readLines(
    unz(tf, filename = "2020_CZ_Region_Mobility_Report.csv")
  ),
  "google/2020_CZ_Region_Mobility_Report.csv"
)

googledata <- read_csv("google/2020_CZ_Region_Mobility_Report.csv")

glimpse(googledata)
skimr::skim(googledata)

gd <- googledata %>%
  select(-sub_region_2, -metro_area, -census_fips_code,
         -country_region, -country_region_code) %>%
  rename(kraj_nazev = sub_region_1,
         kraj_kod_iso = iso_3166_2_code) %>%
  replace_na(replace = list(kraj_nazev = "Celá ČR"))

gd_dlouhe <- gd %>%
  pivot_longer(cols = ends_with("baseline"),
               values_to = "zmena",
               names_to = "kategorie") %>%
  mutate(kategorie = str_remove(kategorie, "_percent_change_from_baseline")) %>%
  mutate(kategorie_cz =
           recode(kategorie,
                  retail_and_recreation = "Retail a rekreace",
                  grocery_and_pharmacy = "Potraviny a lékárny",
                  parks = "Parky",
                  transit_stations = "Stanice veř. dopravy",
                  workplaces = "Pracoviště",
                  residential = "Obydlí"))


max(gd$date)

ptrr::set_ptrr_ggplot_fonts()
ptrr::set_geom_defaults()

vikend_obchody <- gd_dlouhe %>% 
  filter(kraj_nazev == "Celá ČR",
         date > "2020-10-01",
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
       subtitle = "Pokles aktivity v % oproti normálu",
       caption = "@petrbouchal z dat Google Mobility")

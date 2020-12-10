library(tidyverse)

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

Sys.setlocale("LC_ALL", "cs_CZ.UTF-8")

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
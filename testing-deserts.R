library(tidyverse)
library(sf)
library(lubridate)
library(CzechData)

# https://github.com/AmitLevinson/amitlevinson.com/blob/master/content/post/ice-cream-locations/index.Rmd
# https://amitlevinson.com/blog/automated-plot-with-github-actions/

# kolik dostupných do 1.5 km
# jak daleko nejbližší dostupný zítra, popř. jen antigen, popř. s žádankou

tm_phsc_url <- "https://apps.golemio.cz/v1/covid-odberova-mista-api/samplingpoints?lang=cs"
tm_phsc <- st_read(tm_phsc_url) %>% 
  st_transform(5514)

tm_uzis_url <- "https://covid.uzis.cz/server/rest/services/Hosted/covid_forms_tsp_view_public/FeatureServer/0/query?where=*&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=drive_in%2Cname%2Cobjectid%2Creceive_choice&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&having=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=xyFootprint&resultOffset=&resultRecordCount=&returnTrueCurves=false&returnCentroid=false&sqlFormat=none&resultType=&f=json"

tm_uzis <- read_sf(tm_uzis_url) %>% st_transform(5514)

covid_mc_url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/mestske-casti.csv"

covid_mc <- read_csv(covid_mc_url, col_types = cols(nove_pripady = "i",
                                                    aktivni_pripady = "i",
                                                    .default = "c"))

covid_mc_recent <- covid_mc %>% 
  filter(datum > today() - 31) %>% 
  group_by(mc_kod) %>% 
  summarise(nove_pripady = sum(nove_pripady))

adr_phsc <- load_RUIAN_settlement("554782", "adresni mista") %>% 
  st_set_crs(5514)

ph_mc <- load_RUIAN_settlement("554782", "MOMC_P") %>% 
  st_set_crs(5514)

ph_grid <- st_make_grid(adr_phsc, square = F, n = 30) %>% 
  st_as_sf() %>% 
  mutate(hex_id = row_number()) %>% 
  st_intersection(st_union(ph_mc))

ph_sc <- load_RUIAN_state("kraje") %>% 
  filter(nuts3_kod %in% c("CZ010", "CZ020")) %>% 
  st_set_crs(5514)

tm_uzis_ph <- st_intersection(tm_uzis, ph_sc %>% st_union())
tm_uzis_ph <- st_intersection(tm_phsc, ph_sc %>% st_union()) # workaround, using Golemio data

adr_with_nearest <- adr_phsc %>% 
  select(adb_kod = kod, obec_kod, nazev_momc, kod_momc) %>% 
  # sample_n(10000) %>% 
  st_join(tm_phsc, 
          join = st_nearest_feature) %>% 
  left_join(tm_phsc %>% as.data.frame() %>% select(gg = geometry, id)) %>% 
  mutate(nearest_index = st_nearest_feature(geometry, gg),
         dist = st_distance(geometry, gg, by_element = TRUE) %>% 
           units::drop_units(),
         st_within_1500 = st_is_within_distance(geometry, tm_phsc$geometry, dist = 1500) %>% 
           map_int(length))

plot(adr_with_nearest, max.plot = 1)
ggplot(adr_with_nearest %>% 
                  # sample_n(100) %>% 
         filter()
       ) +
  geom_sf(aes(colour = dist), alpha = 0.3, size = 0.5) +
  scale_colour_viridis_c(n.breaks = 8)

dist_by_hex <- adr_with_nearest %>%
  st_join(ph_grid) %>% 
  group_by(hex_id) %>% 
  summarise(mean_dist = mean(dist),
            share_within_1500 = mean(dist < 1500),
            min_n_within_1500 = min(st_within_1500))

dist_by_hex2 <- dist_by_hex %>% 
  st_set_geometry(NULL) %>% 
  left_join(ph_grid) %>% 
  mutate(geometry = x) %>% 
  st_set_geometry("geometry")

ggplot(dist_by_hex2 %>% mutate(min_n_within_1500 = na_if(min_n_within_1500, 0))) +
  geom_sf(data = ph_mc %>% st_union(), fill = "grey", colour = "white") +
  geom_sf(aes(fill = as.factor(min_n_within_1500)), size = 0) +
  # geom_sf(fill = "black", size = 0, data = dist_by_hex2 %>% 
  #           filter(min_n_within_1500 == 0)) +
  scale_fill_viridis_d(name = "Počet míst\ndo 1,5 km", option = "A", na.value = "black", begin = .3) +
  geom_sf(data = ph_mc, fill = NA, colour = "grey") +
  geom_sf(data = tm_phsc %>% st_crop(ph_mc), colour = "white", shape = 4) +
  ptrr::theme_ptrr(panel.grid = element_blank(), 
                   axis.text.y = element_blank(),
                   axis.text.x = element_blank()) +
  labs(title = "Dostupnost testovacích míst v Praze",
       subtitle = "černá = žádné dostupné do 1,5 km, šedá = žádné adresy",
       caption = "Zdroj: covid.praha.eu; ČÚZK\npetrbouchal.xyz/covid")
 
dist_by_mc <- adr_with_nearest %>%
  st_join(ph_grid) %>% 
  mutate(kod_momc = as.character(kod_momc)) %>% 
  group_by(kod_momc) %>% 
  summarise(mean_dist = mean(dist),
            share_within_1500 = mean(dist < 1500), 
            median_n_within_1500 = median(st_within_1500))


pragr::district_hexogram %>% 
  select(kod, label) %>% 
  left_join(dist_by_mc %>% st_set_geometry(NULL), 
            by = c(kod = "kod_momc")) %>% 
  ggplot() +
  geom_sf(aes(fill = share_within_1500)) +
  # ptrr::theme_ptrr(map = TRUE) +
  geom_sf_text(aes(label = label))

ggplot(adr_with_nearest %>% 
         select(dist, kod = kod_momc) %>% 
         drop_na(kod), 
       aes(x = dist/1e3)) +
  geom_density(aes(colour = "blue", fill = after_scale(alpha(colour, .7)))) +
  scale_colour_manual(values = c(blue = "blue"), guide = NULL) +
  theme_ptrr(gridlines = "both", multiplot = T, axis.text.y = element_blank()) +
  facet_geo(~ kod, grid = pragr::district_geofacet, label = "name", scales = "free_y") +
  labs(title = "Vzdálenost k nejbližšímu testovacímu místu")
  

median_age <- pragr::district_age_median %>% 
  filter(sex == "Total", year == max(year))

pop <- pragr::district_age_structure %>% 
  filter(sex == "Total", year == max(year), age == "Total")

mc_over65 <- pragr::district_age_structure %>% 
  filter(sex == "Total", year == max(year), age != "Total") %>% 
  mutate(over65 = as.numeric(age) > 13) %>% 
  group_by(KOD_ZUJ, over65) %>% 
  summarise(n = sum(count)) %>% 
  mutate(share_over65 = n/sum(n)) %>% 
  filter(over65)

mc_all <- pragr::district_hexogram %>% 
  select(kod, label) %>% 
  left_join(dist_by_mc %>% st_set_geometry(NULL), 
            by = c(kod = "kod_momc")) %>% 
  left_join(median_age %>% rename(kod = KOD_ZUJ)) %>% 
  left_join(mc_over65 %>% rename(kod = KOD_ZUJ)) %>% 
  # left_join(nezam_mc, by = c(kod = "code")) %>% 
  left_join(pop %>% rename(kod = KOD_ZUJ)) %>% 
  left_join(covid_mc_recent %>% rename(kod = mc_kod)) %>% 
  mutate(covid_per_100k = nove_pripady/count*100000)

ggplot(mc_all) +
  geom_sf(aes(fill = share_over65)) +
  theme_void() +
  geom_sf_text(aes(label = label))

ggplot(mc_all) +
  geom_point(aes(mean_dist, share_over65))

ggplot(mc_all) +
  geom_point(aes(median_n_within_1500, share_over65))

ggplot(mc_all) +
  geom_point(aes(mean_dist, unemp_rate))

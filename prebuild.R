library(tidyverse)
library(czso)
library(CzechData)

vek <- czso::czso_get_table("130142")
write_csv(vek, "vek.csv")

vek_ciselnik <- czso::czso_get_codelist("cis7700")
write_csv(vek_ciselnik, "vek_ciselnik.csv")

okresy_p <- CzechData::load_RUIAN_state("okresy") %>%
  ms_simplify(keep = .01, keep_shapes = T)
write_rds(okresy_p, "okresy_polygons.rds")
library(tidyverse)
library(czso)
library(CzechData)

vek <- czso::czso_get_table("130142")
write_rds(vek, "vek.rds")

vek_ciselnik <- czso::czso_get_codelist("cis7700")
write_rds(vek_ciselnik, "vek_ciselnik.rds")

okresy_p <- CzechData::load_RUIAN_state("okresy") %>%
  ms_simplify(keep = .01, keep_shapes = T)
write_rds(okresy_p, "okresy_polygons.rds")
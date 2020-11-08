library(tidyverse)
library(czso)
library(CzechData)


okresy_p <- CzechData::load_RUIAN_state("okresy") %>%
  ms_simplify(keep = .01, keep_shapes = T)
write_rds(okresy_p, "okresy_polygons.rds")
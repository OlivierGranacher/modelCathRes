# Creating example data from raw data in R/sysdata.rda
library(dplyr, warn.conflicts = F)
d <- readRDS("data-raw/dj.rds")
resCathData <- d %>%
  na.omit() %>%
  select(group, cod_cuve, agebsq, rucv)
usethis::use_data(resCathData,
  overwrite = T
)

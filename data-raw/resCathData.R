# Creating example data from raw data in R/sysdata.rda
library(dplyr, warn.conflicts = F)
d <- readRDS("data-raw/dj.rds")
d %>%
  group_by(cod_bsq) %>%
  summarise(maxAge = max(agebsq)) %>%
  arrange(desc(maxAge)) %>%
  head(10) %>%
  inner_join(d, "cod_bsq") %>%
  na.omit() %>%
  select(cod_cuve, agebsq, rucv) -> resCathData
usethis::use_data(resCathData,
  overwrite = T
)

# Creating example data from raw data in R/sysdata.rda
load("R/sysdata.rda")
d %>%
  group_by(cod_bsq) %>%
  summarise(maxAge = max(agebsq)) %>%
  arrange(desc(maxAge)) %>%
  head(10) %>%
  inner_join(d, "cod_bsq") %>%
  na.omit() %>%
  select(cod_cuve, agebsq, rucd) -> resCathData
  usethis::use_data(resCathData,
                    overwrite = T)

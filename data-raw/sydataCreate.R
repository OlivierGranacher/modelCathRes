# Creation of sysdata.rda containing dj.rds
d <- readRDS("data-raw/dj.rds")
usethis::use_data(d,
                  internal = TRUE,
                  overwrite = TRUE)
# load("R/sysdata.rda") to load d dataframe

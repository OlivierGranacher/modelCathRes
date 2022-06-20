## Test of findLogFunction
# See vignette validateLogModel.Rmd for generated data
library(dplyr)
df <- read.csv("generated_res_data.csv") %>%
  filter(group == "A")
res <- findLogFunction(df,
                age = age,
                res = cath_res,
                coeff = T
)
expect_equal(signif(res[[1]], 3), 0.598)
expect_equal(signif(res[[2]], 3), 0.0202)

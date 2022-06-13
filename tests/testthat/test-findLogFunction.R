## Test of findLogFunction
# See vignette validateLogModel.Rmd for generated data
df <- read.csv("generated_res_data.csv")
res <- findLogFunction(df,
                age = age,
                res = cath_res,
                coeff = T
)
expect_equal(res[[1]], 0.59702924)
expect_equal(res[[2]], 0.020290598)

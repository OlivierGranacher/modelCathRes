# Test of resCathData
data(resCathData)
d <- resCathData
testthat::expect_output(str(d), "tibble")
testthat::expect_output(str(d), "cuv")
testthat::expect_output(str(d), "age")
testthat::expect_output(str(d), "rucv")
testthat::expect_gt(nrow(d), 3)

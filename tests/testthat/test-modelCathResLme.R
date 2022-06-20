# test modelCathResLme
df2 <- read.csv("generated_res_data.csv")
modLmer <- modelCathoResLme(
  df2,
  group,
  age,
  cath_res
)
# Résultats du modèle
expect_equal(signif(coef(modLmer)$group[, 1], 3), c(0.598, 0.503))
expect_equal(signif(coef(modLmer)$group[, 2], 3), c(0.0202, 0.0295))

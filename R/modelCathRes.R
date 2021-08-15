#' modelCathoRes1
#'
#' Model cathode resistance per groups assuming
#' evolution with log of age and 0 fixed intercept - using lme4
#'
#' @param d data.frame with data
#' @param group char with group names
#' @param age cell age
#' @param res cathode resistance value
#'
#' @return summary of model using broom::tidy
#' @export
#'s
#'
modelCathoRes1 <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ log(", names(dplyr::select(d, {{age}})),
                 ") + ",
                 names(dplyr::select(d, {{group}})),
                 " + 0"
                 )
  broom::tidy(stats::lm(formula = form, d))
}

#' modelCathoRes2
#'
#' Model cathode resistance per groups assuming
#' the same evolution with age and 0 intercept - using lme4
#'
#' @param d data.frame with data
#' @param group char with group names
#' @param age cell age
#' @param res cathode resistance value
#'
#' @return lmer model
#' @export
#'
#'
modelCathoRes2 <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ 0 + (1 + log(", names(dplyr::select(d, {{age}})),
                 ") | ",
                 names(dplyr::select(d, {{group}})),
                 ") "
                 )
  # DAtaframe of random coefficients
  lme4::lmer(form, d)
}


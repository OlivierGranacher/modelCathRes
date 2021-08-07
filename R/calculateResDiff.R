#' calculateResDiff : calculates resistance difference
#'
#' Calculates the difference in cathode resistance between cathode resistance groups assuming
#' the same evolution with age and 0 intercept
#'
#' @param d data.frame with data
#' @param group char with group names
#' @param age cell age
#' @param res cathode resistance value
#'
#' @return summary of model using broom::tidy
#' @exports
#'s
#'
calculateResDiff <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ log(", names(dplyr::select(d, {{age}})),
                 ") + ",
                 names(dplyr::select(d, {{group}})),
                 " - 1"
                 )
  broom::tidy(stats::lm(formula = form, d))
}

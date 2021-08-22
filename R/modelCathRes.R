#' modelCathoLm
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
#'
#'
modelCathoResLm <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ log(", names(dplyr::select(d, {{age}})),
                 ") + ",
                 names(dplyr::select(d, {{group}})),
                 " + 0"
                 )
  broom::tidy(stats::lm(formula = form, d))
}

#' modelCathoResLme
#'
#' Model cathode resistance per groups with  0 fixed intercept and varying (random)
#' intercept and slope
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
modelCathoResLme <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ 0 + (1 + log(", names(dplyr::select(d, {{age}})),
                 ") | ",
                 names(dplyr::select(d, {{group}})),
                 ") "
                 )
  # Dataframe of random  coefficients
  lme4::lmer(form, d)
}

#' modelCathoResLmeStan
#'
#' Model cathode resistance per groups with  0 fixed intercept and varying (random)
#' intercept and slope
#'
#' @param d data.frame with data
#' @param group char with group names
#' @param age cell age
#' @param res cathode resistance value
#'
#'@importfrom rstanarm stan_glmer
#'
#' @return Stan lmer model
#' @export
#'
#'
modelCathoResLmeStan <- function(d, group, age, res) {
  form <- paste0(names(dplyr::select(d, {{res}})),

                 " ~ 0 + (1 + log(", names(dplyr::select(d, {{age}})),
                 ") | ",
                 names(dplyr::select(d, {{group}})),
                 ") "
                 )

  # Prior setting
  priorNorm <- rstanarm::normal(location = c(0.5, 0.025), scale = c(0.3, 0.02))
  # Model
  rstanarm::stan_glmer(formula = form,
                      data = d,
                      prior = priorNorm,
                      chains = 2,
                      iter = 100,
                      cores = parallel::detectCores()
                      )
}

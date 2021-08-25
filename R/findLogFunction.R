#' findLogFunction
#'
#' Calculates the log function interpolating cathode resistance data
#' resistance = a + b * log(age)
#'
#' @param d datatable containing the data
#' @param age numeric pot age in days
#' @param res numeric cathode resistance in microhm
#' @param coeff logical if FALSE (default) return function, if TRUE returns vector c(a, b)
#'
#' @return function
#' @export
#'
#' @examples
#' data("resCathData")
#' d <- resCathData
#' findLogFunction(d, agebsq, rucv)
#'
#'
findLogFunction <- function(d, age, res, coeff = F) {
# Formula a + b log(x)
form <- paste0(names(dplyr::select(d, {{res}})), " ~ log(", names(dplyr::select(d, {{age}})), ")")
modlm <- stats::lm(formula = form, d)
a <- summary(modlm)$coefficients[[1]]
b <- summary(modlm)$coefficients[[2]]
logFun <- function(x, a1 = a, b1 = b){
  a1 + b1 * log(x)
}
if (!coeff) return(logFun) else return(c(a, b))
}

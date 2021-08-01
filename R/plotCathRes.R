#' plotCathRes
#'
#' plots cathode resistance per pot for several pots  versus age
#'
#' @param d datatable containing the data
#' @param pot character pot variable
#' @param age numeric pot age in days
#' @param res numeric cathode resistance in microhm
#' @param ncol number of columns for facetting
#'
#' @return ggplot with a facet for each pot
#' @export
#'
#' @examples
#' data("resCathData")
#' d <- resCathData
#' plotCathRes(d, cod_cuve, agebsq, rucv)
#'
#'
plotCathRes <- function(d, pot, age, res, ncol = 3) {
ggplot2::ggplot(data = d, ggplot2::aes(x = {{age}}, y ={{res}})) +
    ggplot2::geom_point(shape = 21, fill = NA, size = .9) +
    ggplot2::facet_wrap(facets = dplyr::vars({{pot}}),ncol =  ncol)
}

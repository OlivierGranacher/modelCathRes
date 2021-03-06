\name{resCathData}
\alias{resCathData}
\docType{data}
\title{resCathData : cathode resistance data}
\description{
Dataframe with 4 columns with cathode resistance data.
}
\usage{data("resCathData")}
\format{
  A data frame with 13938 observations of the following 4 variables.
  \describe{
   \item{\code{group}}{character, group}
    \item{\code{cod_cuve}}{character, cell code }
    \item{\code{agebsq}}{numeric, cell age in days}
    \item{\code{rucv}}{numeric, cathode resistance in micro-ohm}
  }
}
\examples{
data(resCathData)
str(resCathData)
}
\keyword{datasets}

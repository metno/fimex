\name{mifi.reader.getProj4}
\alias{mifi.reader.getProj4}
\title{
mifi.reader.getProj4-internals
}
\description{
Get the identified projection as proj4-string for a variable.
}
\usage{
mifi.reader.getProj4(reader, varName)
}
\arguments{
  \item{reader}{
  a reader as retrieved from mifi.reader.new
}
  \item{varName}{
    a variable name as retrieved from mifi.reader.variables
}
}
\details{
This will try to identify the coordinate-system with the variable, and return the projection as proj4-string
}
\references{
}
\author{
Heiko Klein
}
\note{

}


\seealso{
\code{\link{mifi.reader.new}},
\code{\link{mifi.reader.variables}},
\code{\link{mifi.sb.getDimensions}}
}
\examples{
# R example for RFimex
library(RFimex)

reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}
proj4 <- mifi.reader.getProj4(reader, "altitude")
# proj4 = "+proj=latlon +R=3671000"

}

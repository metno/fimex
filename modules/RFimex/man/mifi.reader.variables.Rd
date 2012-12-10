\name{mifi.reader.variables}
\alias{mifi.reader.variables}
\title{
mifi.reader.variables-internals
}
\description{
    fetch a vector of available variables from a reader
}
\usage{
mifi.reader.variables(reader)
}
\arguments{
  \item{reader}{
  a reader as retrieved from mifi.reader.new, or a derivated reader like mifi.reader.lonLatInterpolated
}
}
\details{
    Lists all, also auxiliary variable-names of a source.
}
\value{
}
\references{
}
\author{
Heiko Klein
}
\note{
}

\seealso{
\code{\link{mifi.reader.new}}, \code{\link{mifi.reader.lonLatInterpolated}}
}
\examples{
library(RFimex)

## The function is currently defined as
reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}

vars <- mifi.reader.variables(reader)

}

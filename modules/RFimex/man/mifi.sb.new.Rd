\name{mifi.sb.new}
\alias{mifi.sb.new}
\title{
mifi.sb.new-internals
}
\description{
Prepare an SliceBuilder for fetching a data-slice from a variable.
}
\usage{
mifi.sb.new(reader, varName)
}
\arguments{
  \item{reader}{
    a reader, e.g. fetched from mifi.reader.new
}
  \item{varName}{
    a variable name, e.g. one of those given in mifi.reader.variables
}
}
\details{
}
\value{
Returns a slicebuilder object.
}
\references{
}
\author{
Heiko Klein
}
\note{
}

\seealso{
\code{\link{mifi.reader.new}},\code{\link{mifi.reader.variables}}
}
\examples{
# R example for RFimex
library(RFimex)


reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}

sb <- mifi.sb.new(reader,"time")

}

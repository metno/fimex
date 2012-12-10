\name{mifi.sb.getDimensions}
\alias{mifi.sb.getDimensions}
\title{
mifi.sb.getDimensions-internals
}
\description{
List all dimensions used in a SliceBuilder 
}
\usage{
mifi.sb.getDimensions(sb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sb}{
    SliceBuilder as retrieved from mifi.sb.new
}
}
\details{
}
\value{
List of all dimensions of the variable given to the SliceBuilder
}
\references{
}
\author{
Heiko Klein
}
\note{
}

\seealso{
\code{\link{mifi.reader.new}}, \code{\link{mifi.sb.new}}
}
\examples{
# R example for RFimex
library(RFimex)


reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}

sb <- mifi.sb.new(reader,"time")
dims <- mifi.sb.getDimensions(sb)
# dims contains now just "time" dimension
}

\name{mifi.reader.uniqueRefTime}
\alias{mifi.reader.uniqueRefTime}
\title{
mifi.reader.uniqueRefTime-internals
}
\description{
    fetch a vector of available uniqueRefTime from a reader
}
\usage{
mifi.reader.uniqueRefTime(reader, units)
}
\arguments{
  \item{reader}{
  a reader as retrieved from mifi.reader.new, or a derivated reader like mifi.reader.lonLatInterpolated
}
  \item{units}{
  a time unit, like "seconds since 1970-01-01 00:00:00 +0000"
}
}
\details{
    Lists all, also auxiliary variable-names of a source.
}
\value{
    the unique reference time of the file, if only one available
}
\references{
}
\author{
Heiko Klein
}
\note{
}

\seealso{
\code{\link{mifi.reader.new}}
}
\examples{
library(RFimex)

## The function is currently defined as
reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}

refTime <- mifi.reader.uniqueRefTime(reader, "seconds since 1970-01-01 00:00:00 +0000")

}

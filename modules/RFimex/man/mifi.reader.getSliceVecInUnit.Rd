\name{mifi.reader.getSliceVecInUnit}
\alias{mifi.reader.getSliceVecInUnit}
\title{
mifi.reader.getSliceVecInUnit-internals
}
\description{
Fetch a data, or a data-slice from the data-source.
}
\usage{
mifi.reader.getSliceVecInUnit(reader, varName, sb, units)
}
\arguments{
  \item{reader}{
    a reader, e.g. from mifi.reader.new
}
  \item{varName}{
    a variable name
}
  \item{sb}{
    a slice-builder, belonging to the variable name, eventually redused by mifi.sb.setStartAndSize
}
  \item{units}{
    units to fetch the data from. units will be automatically converted. If the original data does not
    have units, fetching will fail.
}
}
\details{
}
\value{
A data-array.
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
mifi.sb.setStartAndSize(sb,"time", 3, 5)

time <- mifi.reader.getSliceVecInUnit(reader, "time", sb, "seconds since 2007-05-16 00:00:00 +0000")
# time has now size 5 and starts from the 4th position in the file
}

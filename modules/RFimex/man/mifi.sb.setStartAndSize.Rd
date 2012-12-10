\name{mifi.sb.setStartAndSize}
\alias{mifi.sb.setStartAndSize}
\title{
mifi.sb.setStartAndSize-internals
}
\description{
Prepare data-fetching to slice data along a dimension.
}
\usage{
mifi.sb.setStartAndSize(sb, dimName, start, size)
}
\arguments{
  \item{sb}{
a slicebuilder as retrieved from mifi.sb.new
}
  \item{dimName}{
dimension name of the variable of the slicebuilder
}
  \item{start}{
start-position of the slice, starting at 0
}
  \item{size}{
size of the new slice
}
}
\details{
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
\code{\link{mifi.sb.new}},
\code{\link{mifi.reader.getSliceVecInUnit}}
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

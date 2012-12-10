\name{mifi.reader.write}
\alias{mifi.reader.write}
\title{
mifi.reader.write-internals
}
\description{
    Write a reader to a file, currently only netcdf-files implemented.
}
\usage{
mifi.reader.write(reader, type, filename, configname = "")
}

\arguments{
  \item{reader}{
  a reader as retrieved from mifi.reader.new, or a derivated reader like mifi.reader.lonLatInterpolated
}
  \item{type}{
filetype, must be "netcdf", "netcdf3" or "netcdf4" currently
}
  \item{filename}{
filename of the created file
}
  \item{configname}{
  optional netcdf-config file
}
}
\details{
    returns nothing
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

status <- try(mifi.reader.write(reader, "netcdf", "outTest.nc"))
if (inherits(status, "try-error")) {
    stop("mifi.reader.write failed")
}

}

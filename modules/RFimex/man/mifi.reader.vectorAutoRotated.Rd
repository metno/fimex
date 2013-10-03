\name{mifi.reader.vectorAutoRotated}
\alias{mifi.reader.vectorAutoRotated}
\title{
mifi.reader.vectorAutoRotated-internals
}
\description{
Read data in original grid, but with vectors rotated to the grid, or to latlon
}
\usage{
mifi.reader.vectorAutoRotated(reader, toLatLon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reader}{
    a CDMReader as created by mifi.reader.new
  }
  \item{toLatLon}{
    a value of 0 will rotate the lat/lon vectors to the grid,
    a value of 1 will rotate the x/y-vectors to north/east
  }
}
\details{
The data-fetching is only prepared in this step. Vectors are rotated during the fetch, e.g. mfif.reader.getSliceVecInUnit
}

\value{
    a CDMReader object
}
\references{
}
\author{
Heiko Klein
}
\note{
Vector properties, like wind or current are rotated if the spatial_direction property is set
in the setup file, or if the standard_name is known, e.g. x_wind, *_x_velocity, *_x_transport,
*_x_displacement. The complete list can be seen in https://svn.met.no/viewvc/fimex/trunk/src/coordSys/CF1_xCoordSysBuilder.cc?view=markup
(enhanceVectorProperties)
}

\seealso{
\code{\link{mifi.reader.new}}
}
\examples{
library(RFimex)

reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}
iread <- mifi.reader.vectorAutoRotated(1);

}

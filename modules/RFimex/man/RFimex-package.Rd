\name{RFimex-package}
\alias{RFimex-package}
\alias{RFimex}
\docType{package}
\title{
Access to gridded data via Fimex
}

\description{
The RFimex package exposes some of the Fimex library functionality
to access gridded data, eventually interpolated gridded data.
}

\details{
\tabular{ll}{
Package: \tab RFimex\cr
Type: \tab Package\cr
Version: \tab 0.05\cr
Date: \tab 2015-02-26\cr
License: \tab LGPL-2 \cr
}

}

\author{
Heiko Klein
Maintainer: Heiko Klein <heiko.klein@met.no>
}
\references{
http://fimex.met.no
}

\keyword{ package }
\seealso{
\code{\link{mifi.reader.new}},
\code{\link{mifi.reader.lonLatInterpolated}},
\code{\link{mifi.reader.vectorAutoRotated}},
\code{\link{mifi.reader.variables}},
\code{\link{mifi.reader.getCoordinates}},
\code{\link{mifi.reader.getProj4}},
\code{\link{mifi.reader.getSliceVecInUnit}},
\code{\link{mifi.reader.write}},
\code{\link{mifi.sb.new}},
\code{\link{mifi.sb.getDimensions}},
\code{\link{mifi.sb.getStartAndSize}},
}

\examples{
library(RFimex)
# real world reader
reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}
#reader <- mifi.reader.new("netcdf", "out.nc", "")
#reader <- mifi.reader.new("netcdf", "http://thredds.met.no/thredds/dodsC/metno/proff4km/default/Proff_Default_4km_best.ncd", "")

vars <- mifi.reader.variables(reader)
# check if "altidude" is part of vars
# ...

altCoords <- mifi.reader.getCoordinates(reader, "altitude")
sb <- mifi.sb.new(reader, "altitude")
if (!is.na(altCoords["time"])) {
    # just fetch first time
    mifi.sb.setStartAndSize(sb, altCoords["time"], 0, 1)
}
# just fetch on corner of size 3 * 5
if (!is.na(altCoords["x"])) {
    mifi.sb.setStartAndSize(sb, altCoords["x"], 0, 3)
}
if (!is.na(altCoords["y"])) {
    mifi.sb.setStartAndSize(sb, altCoords["y"], 0, 5)
}
# altitude data in cm
altData <- mifi.reader.getSliceVecInUnit(reader, "altitude", sb, "cm")
if (length(altData) != 15) {
    stop("didn't get all data; got ", length(altData))
}

altProj4 <- mifi.reader.getProj4(reader, "altitude")
if (altProj4 == "") {
    stop("unable to read proj4-string for altitude; got ", altProj4)
}


}

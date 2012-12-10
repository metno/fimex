\name{mifi.reader.lonLatInterpolated}
\alias{mifi.reader.lonLatInterpolated}
\title{
mifi.reader.lonLatInterpolated-internals
}
\description{
Read data interpolated to a list of latitude longitude points from a datasource
}
\usage{
mifi.reader.lonLatInterpolated(reader, method, lons, lats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reader}{
    a CDMReader as created by mifi.reader.new
  }
  \item{method}{
    interpolation method, see http://fimex.met.no/doc/mifi__constants_8h_source.html MIFI_INTERPOL_* constants, e.g.
     NEAREST_NEIGHBOR, BILINEAR, BICUBIC, COORD_NN, COORD_NN_KD, FORWARD_SUM, FORWARD_MEAN, FORWARD_MEDIAN, FORWARD_MAX,
     FORWARD_MIN
  }
  \item{lons}{
  vector of longitude values in degree
  }
  \item{lats}{
  vector of latitude values in degree, must be same length as lons
  }
}
\details{
The data-fetching is only prepared in this step. Data is interpolated during data-fetching, e.g. mfif.reader.getSliceVecInUnit
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
Vector properties, like wind or current are currently kept in grid-direction. This might change without notice in the future.
The output will be of size: x=length(lons), y=1
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
lats <- c(70:90);
lons <- c(-10:10);
iread <- mifi.reader.lonLatInterpolated(reader, 1, lons, lats);

}

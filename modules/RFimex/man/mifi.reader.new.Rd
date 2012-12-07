\name{mifi.reader.new}
\alias{mifi.reader.new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mifi.reader.new-internals
}
\description{
    create a new reader object, e.g. a file-handle
}
\usage{
mifi.reader.new(type, dataFile, configFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
    file-type, e.g. felt, netcdf, grib
  }
  \item{dataFile}{
    source of data, e.g. filename or file-URL (in the case of opendap)
  }
  \item{configFile}{
    configuration of dataFile, required for felt and grib-files, optional for netcdf
  }
}
\details{
}

\value{
    Returns a CDMReader object. This function might fail, use try{}
}
\references{
}
\author{
    Heiko Klein
}
\note{
}

\seealso{
}
\examples{
library(RFimex)

## The function is currently defined as
reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}

## examples with netcdf or opendap (without "try")
#reader <- mifi.reader.new("netcdf", "out.nc", "")
#reader <- mifi.reader.new("netcdf", "http://thredds.met.no/thredds/dodsC/metno/proff4km/default/Proff_Default_4km_best.ncd", "")

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

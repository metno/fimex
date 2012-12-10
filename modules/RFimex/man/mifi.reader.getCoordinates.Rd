\name{mifi.reader.getCoordinates}
\alias{mifi.reader.getCoordinates}
\title{
mifi.reader.getCoordinates-internals
}
\description{
Get the identified coordinate-dimensions from a variable. 
}
\usage{
mifi.reader.getCoordinates(reader, varName)
}
\arguments{
  \item{reader}{
  a reader as retrieved from mifi.reader.new
}
  \item{varName}{
    a variable name as retrieved from mifi.reader.variables
}
}
\details{
This will try to identify the coordinate-system with the variable, and return all dimensions.
}
\value{
\item{time}{Name of the time-dimension}
\item{x}{Name of the x-dimension (or longitude)}
\item{y}{Name of the y-dimension (or latitude)}
\item{z}{Name of the vertical-dimension (height, sigma, ...)}
\item{refTime}{Name of the reference-time dimension}
\item{other1}{Name of the first unknown dimensions}
\item{other2}{Name of the 2. unknown dimensions}
\item{other3}{Name of the 3. unknown dimensions}
\item{other4}{Name of the 4. unknown dimensions}
...
}
\references{
}
\author{
Heiko Klein
}
\note{

}

\section{Warning }{
If no dimensions are return, this does not mean, that the variable is dimension-less. It just means, that no
coordinate-system could be found.
}

\seealso{
\code{\link{mifi.reader.new}}, 
\code{\link{mifi.reader.variables}},
\code{\link{mifi.sb.getDimensions}}  
}
\examples{
# R example for RFimex
library(RFimex)

reader <- try(mifi.reader.new("felt", "../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}
coords <- mifi.reader.getCoordinates(reader, "altitude")
# coords = c(x = x, y = y) 

}

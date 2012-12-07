# R example for RFimex
library(RFimex)


# create a non-working reader / check exeption-handling
reader <- try(mifi.reader.new("felt", "does not exist", "me neither"))
if (! inherits(reader, "try-error")) {
    stop("mifi.reader.new should fail here")
}

# real world reader
reader <- try(mifi.reader.new("felt", "../../../test/flth00.dat", "../../../share/etc/felt2nc_variables.xml"))
if (inherits(reader, "try-error")) {
    stop("mifi.reader.new failed")
}
#reader <- mifi.reader.new("netcdf", "out.nc", "")
#reader <- mifi.reader.new("netcdf", "http://thredds.met.no/thredds/dodsC/metno/proff4km/default/Proff_Default_4km_best.ncd", "")

vars <- mifi.reader.variables(reader)
length(vars)
vars
if (length(vars) != 21) {
    stop("didn't get all variables, just ",vars);
} 

coords <- mifi.reader.getCoordinates(reader, "altitude")
coords
if (length(coords) != 2) {
    stop("wrong coordinates for altitude: ", coords);
}
if (coords["x"] != "x") {
    stop("wrong coordinates for altitude: ", coords);
}
coords <- mifi.reader.getCoordinates(reader, "precipitation_amount")
if (length(coords) != 3) {
    stop("wrong coordinates for precipitation_amount: ", coords);
}
coords <- mifi.reader.getCoordinates(reader, "time")
if (! is.null(coords)) {
    stop("time is coordinate axes, and should not have a coordinate system")
}



sb <- mifi.sb.new(reader,"time")
cat(class(sb))
dims <- mifi.sb.getDimensions(sb)
dims
mifi.sb.setStartAndSize(sb, "time", 0, 3)
# TODO: throws exception: mifi.sb.setStartAndSize(sb, "x", 0, 3)
#out <- array(0, dim=c(3))
#out <- double(length = 3)
time <- mifi.reader.getSliceVecInUnit(reader, "time", sb, "seconds since 2007-05-16 00:00:00 +0000")
if (length(time) != 3) {
    stop("didn't get all data, just ",length(time));
}


sb <- mifi.sb.new(reader,"altitude")
#mifi.sb.setStartAndSize(sb, "x", 0, 3)
#mifi.sb.setStartAndSize(sb, "y", 5, 3)
altitude <- mifi.reader.getSliceVecInUnit(reader, "altitude", sb, "m")
if (length(altitude) != 44884) {
    stop("didn't get all data, just ",length(altitude));
}

status <- try(mifi.reader.write(reader, "netcdf", "outTest.nc"))
if (inherits(status, "try-error")) {
    stop("mifi.reader.write failed")
}

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

sb <- SliceBuilder(boost__shared_ptrCDMReader_getCDM(reader),"time")
cat(class(sb))
SliceBuilder_setStartAndSize(sb, "time", 0, 3)
# TODO: throws exception: SliceBuilder_setStartAndSize(sb, "x", 0, 3)
#out <- array(0, dim=c(3))
#out <- double(length = 3)
time <- boost__shared_ptrCDMReader_getSliceVecInUnit(reader, "time", sb, "seconds since 2007-05-16 00:00:00 +0000")
#time
time[1]
time[2]
time[3]
cat(length(time))
cat(time$"__len__"())
out = array(0, c(3))
out[1:3] <- time[1:time$"__len__"()]
cat(length(out))
out


sb <- SliceBuilder(boost__shared_ptrCDMReader_getCDM(reader),"altitude")
#SliceBuilder_setStartAndSize(sb, "x", 0, 3)
#SliceBuilder_setStartAndSize(sb, "y", 5, 3)
altitude <- boost__shared_ptrCDMReader_getSliceVecInUnit(reader, "altitude", sb, "m")

#out <- array(0, c(229, 196))
#out <- altitude[1:44884]
out <- mifi.vec.2R(altitude);
out$status
out$errmsg
length(out$data)
#altitude[1]
#out


status <- try(mifi.reader.write(reader, "netcdf", "outTest.nc"))
if (inherits(status, "try-error")) {
    stop("mifi.reader.write failed")
}

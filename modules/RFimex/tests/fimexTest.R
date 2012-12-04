#! /usr/bin/R
library(RFimex)

reader <- CDMFileReaderFactory_create("felt", "../../../test/flth00.dat", "../../../share/etc/felt2nc_variables.xml")
#reader <- CDMFileReaderFactory_create("netcdf", "out.nc", "")
#reader <- CDMFileReaderFactory_create("netcdf", "http://thredds.met.no/thredds/dodsC/metno/proff4km/default/Proff_Default_4km_best.ncd", "")

sb <- SliceBuilder(boost__shared_ptrCDMReader_getCDM(reader),"time")
cat(class(sb))
SliceBuilder_setStartAndSize(sb, "time", 0, 3)
# throws exception: SliceBuilder_setStartAndSize(sb, "x", 0, 3)
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


#out <- NetCDF_CDMWriter(reader, "outTest.nc")
#delete_NetCDF_CDMWriter(out)

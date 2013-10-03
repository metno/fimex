# public functions of the RFimex module

mifi.interpolTypes <- function(type) {
    switch (type,
    NEAREST_NEIGHBOR = 0,
    BILINEAR         = 1,
    BICUBIC          = 2,
    COORD_NN         = 3,
    COORD_NN_KD      = 4,
    FORWARD_SUM      = 5,
    FORWARD_MEAN     = 6,
    FORWARD_MEDIAN   = 7,
    FORWARD_MAX      = 8,
    FORWARD_MIN      = 9,
    stop("unknown interpolType: ", type)
    )
}

mifi.vec.2R <- function(vec) {
    if ( extends(class(vec), '_p_std__vectorT_double_std__allocatorT_double_t_t' ) ) {
       ; ans <- .Call("mifi_Rdoublevector", vec, PACKAGE='RFimex');
    } else {
       stop("cannot find overloaded function for mifi.vec.2R and type", toString(class(vec)));
    };
    ans
}

mifi.createCDMReader_ <- function(r) {
    # enhance the reader with coordinate-systems
    ans <- c( p_ = r, csList_ = listCoordinateSystems(r) );
    class(ans) <- c("CDMReader");
    ans
}

mifi.reader.new <- function(type, filename, config = "") {
    r <- CDMFileReaderFactory_create(type, filename, config);
    if (is.null(r)) {
        stop("unable to create reader with mifi.reader.create(",type,",",filename,",",config,")");
    }
    mifi.createCDMReader_(r)
}

mifi.reader.lonLatInterpolated <- function(reader, method, lons, lats) {
    if (length(lats) != length(lons)) {
        stop("lats size must be == lons size");
    }
    latV <- DoubleVector(length(lats));
    lonV <- DoubleVector(length(lons));
    for (i in 1:length(lats)) {
        DoubleVector___setitem__(latV, i-1, lats[i]);
        DoubleVector___setitem__(lonV, i-1, lons[i]);
    }
    methodId <- mifi.interpolTypes(method);
    r <- latLonInterpolatedReader(reader$p_, methodId, lonV, latV);
    if (is.null(r)) {
        stop("interpolation to latlon values failed");
    }
    mifi.createCDMReader_(r)
}

mifi.reader.vectorAutoRotated <- function(reader, toLatLon) {
    r <- vectorAutoRotatedReader(reader$p_, toLatLon);
    if (is.null(r)) {
        stop("interpolation to latlon values failed");
    }
    mifi.createCDMReader_(r)
}


mifi.reader.variables <- function(reader) {
    mifiRead <- mifi_cdm_reader(reader$p_);
    count <- mifi_get_variable_number(mifiRead);
    if (count == 0) {
        stop ("no variables found in reader");
    }
    ans <- c();
    for (i in 0:(count-1)) {
        ans[i+1] = mifi_get_variable_name(mifiRead, i);
    }
    ans
}

mifi.reader.getCoordinates <- function(reader, varName) {
    # will give logical coordinates, like time, x(lon), y(lat), z, refTime, offsetTime, other1, other2, ...
    coordVec <- listCoordinates(reader$p_, reader$csList_, varName);
    ans = c();
    if (coordVec$"__len__"() > 0) {
        for (i in 1:(coordVec$"__len__"())) {
            if (coordVec[i] != "") {
                if (i == 1) {
                    ans["time"] = coordVec[i];
                } else if (i == 2) {
                    ans["x"] = coordVec[i];
                } else if (i == 3) {
                    ans["y"] = coordVec[i];
                } else if (i == 4) {
                    ans["z"] = coordVec[i];
                } else if (i == 5) {
                    ans["refTime"] = coordVec[i];
                } else {
                    otherI = as.character(i-5);
                    other = paste(c("other",otherI), collapse='');
                    ans[other] = coordVec[i];
                }
            }
        }
    }
    ans
}

mifi.reader.uniqueRefTime <- function(reader, units = "seconds since 1970-01-01 00:00:00 +0000") {
    retVal <- mifi_get_unique_forecast_reference_time(reader$p_, units);
    if (is.nan(retVal)) {
        stop("error on mifi.reader.uniqueRefTime(reader, ",units,")");
    }
    retVal
}

mifi.sb.new <- function(reader, varName) {
    SliceBuilder(boost__shared_ptrCDMReader_getCDM(reader$p_), varName);
}
mifi.sb.getDimensions <- function(sb) {
    dimNames <- SliceBuilder_getDimensionNames(sb);
    dimSizes <- SliceBuilder_getDimensionSizes(sb);
    dims <- c();
    for (i in 1:StringVector___len__(dimNames)) {
        dims[dimNames[i]] <- dimSizes[i];
    }
    dims
}

mifi.sb.setStartAndSize <- function(sb, dimName, start, size) {
    SliceBuilder_setStartAndSize(sb, dimName, start,size)
}

mifi.reader.getSliceVecInUnit <- function(reader, varName, sb, units = "") {
    out <- boost__shared_ptrCDMReader_getSliceVecInUnit(reader$p_, varName, sb, units);
    if (is.null(out)) {
        stop("unable to get data for mifi.reader.getSliceVecInUnit(reader, ",varName,",sb, ",units,")");
    }
    outVec <- mifi.vec.2R(out);
    if (outVec$status != 0) {
        stop("unable to convert vector: ", outVec$errmsg);
    }
    outVec$data
}

mifi.reader.write <- function(reader, type, filename, configname = "") {
    if (type == "netcdf" || type == "netcdf3") {
        out <- NetCDF_CDMWriter(reader$p_, filename, configname, 3);
        if (is.null(out)) {
            stop("NetCDF_CDMWriter(",reader$p_,",", filename,",", configname,", 3) failed");
        } else {
            delete_NetCDF_CDMWriter(out);
        }
    } else if (type == "netcdf4") {
        out <- NetCDF_CDMWriter(reader$p_, filename, configname, 3);
        if (is.null(out)) {
            stop("NetCDF_CDMWriter(",reader,",", filename,",", configname,", 3) failed");
        } else {
            delete_NetCDF_CDMWriter(out);
        }
    } else {
        stop("cannot write unknown type: ", type);
    }
    1
}

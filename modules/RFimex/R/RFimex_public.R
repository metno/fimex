# public functions of the RFimex module

mifi.vec.2R <- function(vec) {
    if ( extends(class(vec), '_p_std__vectorT_double_std__allocatorT_double_t_t' ) ) {
       ; ans <- .Call("mifi_Rdoublevector", vec, PACKAGE='RFimex');
    } else {
       stop("cannot find overloaded function for mifi.vec.2R and type", toString(class(vec)));
    };
    ans
}

mifi.reader.new <- function(type, filename, config = "") {
    ans <- CDMFileReaderFactory_create(type, filename, config);
    if (is.null(ans)) {
        stop("unable to create reader with mifi.reader.create(",type,",",filename,",",config,")");
    }
    ans 
}

mifi.reader.variables <- function(reader) {
    mifiRead <- mifi_cdm_reader(reader);
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

#mifi.reader.getCoordinates(varName) {
    # will give logical coordinates, like time, x(lon), y(lat), z, refTime, offsetTime, other1, other2, ...
#}

mifi.sb.new <- function(reader, varName) {
    SliceBuilder(boost__shared_ptrCDMReader_getCDM(reader), varName);
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

mifi.reader.getSliceVecInUnit <- function(reader, varName, sb, units) {
    out <- boost__shared_ptrCDMReader_getSliceVecInUnit(reader, varName, sb, units);
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
        out <- NetCDF_CDMWriter(reader, filename, configname, 3);
        if (is.null(out)) {
            stop("NetCDF_CDMWriter(",reader,",", filename,",", configname,", 3) failed");
        } else {
            delete_NetCDF_CDMWriter(out);
        }
    } else if (type == "netcdf4") {
        out <- NetCDF_CDMWriter(reader, filename, configname, 3);
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

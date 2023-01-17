#! /usr/bin/env python3
import pyfimex0
import numpy as np
import xarray
from xarray.backends import BackendEntrypoint, BackendArray
import os


DTYPE_MAPPINGS = {
    pyfimex0.CDMDataType.FLOAT: np.float32,
    pyfimex0.CDMDataType.DOUBLE: np.float64,
    pyfimex0.CDMDataType.INT: np.intc,
    pyfimex0.CDMDataType.INT64: np.int64,
    pyfimex0.CDMDataType.UINT: np.uint,
    pyfimex0.CDMDataType.UINT64: np.uint64,
    pyfimex0.CDMDataType.SHORT: np.short,
    pyfimex0.CDMDataType.USHORT: np.ushort,
    pyfimex0.CDMDataType.STRING: str,
    pyfimex0.CDMDataType.CHAR: np.byte,
    pyfimex0.CDMDataType.UCHAR: np.ubyte,
}


class FimexBackendEntrypoint(BackendEntrypoint):
    def open_dataset(self, filename_or_obj, drop_variables, *, config=None, filetype=None, decode_times=True, scale_offset=True):
        if filetype is None:
            filetype = ""
        if config is None:
            config = ""
        fh = pyfimex0.createFileReader(filetype, filename_or_obj, config)
        cdm = fh.getCDM()

        var_names = set(cdm.getVariableNames())
        attr_names = cdm.getGlobalAttributeNames()

        attrs = dict()
        for aname in attr_names:
            attr = cdm.getGlobalAttribute(aname)
            attrv = attr.getData().values()
            attrs[aname] = attrv

        coord_names = cdm.getDimensionNames()
        coords = dict()
        for cname in coord_names:
            var = cdm.getVariable(cname)
            values = var.getData().values()
            shape = var.getShape()
            if len(shape) != 1:
                raise Exception()

            if scale_offset:
                try:
                    scale_factor = cdm.getAttribute(cname, "scale_factor").getData().values()
                except RuntimeError:
                    scale_factor = 1
                try:
                    add_offset = cdm.getAttribute(cname, "add_offset").getData().values()
                except RuntimeError:
                    add_offset = 0
                values = values*scale_factor + add_offset
            coords[cname] = values
            try:
                var_names.remove(cname)
            except KeyError:
                pass

        vars = dict()
        for vname in var_names:
            var = cdm.getVariable(vname)
            dims = list(reversed(var.getShape()))
            attrs = dict()
            for aname in cdm.getAttributeNames(vname):
                attr = cdm.getAttribute(vname, aname)
                attrv = attr.getData().values()
                if len(attrv) == 1:
                    attrv = attrv[0]
                attrs[aname] = attrv

            data = FimexDataVariable(fh, cdm, var, scale_offset=scale_offset)
            data = xarray.core.indexing.LazilyIndexedArray(data)
            var = xarray.Variable(data=data, dims=dims, attrs=attrs)
            vars[vname] = var

        if decode_times:
            if "time" in coord_names:
                var = coords["time"]
                var = np.array([np.datetime64(int(v), "s") for v in var])

                coords["time"] = var

        ds = xarray.Dataset(data_vars=vars, attrs=attrs, coords=coords)

        return ds

    open_dataset_parameters = ["filename_or_obj", "drop_variables", "config", "filetype", "decode_times", "scale_offset"]

    def guess_can_open(self, filename_or_obj):
        try:
            _, ext = os.path.splitext(filename_or_obj)
        except TypeError:
            return False

        return ext in {".grbml", ".ncml", ".grb", ".nc"}

    description = "Read using fimex"
    url = "https://github.com/metno/fimex"


class FimexDataVariable(BackendArray):
    def __init__(self, fh, cdm, var, scale_offset):
        super().__init__()
        self.fh = fh
        self.var = var
        self.cdm = cdm
        self.data = var.getData()
        self.scale_offset = scale_offset

        shape = []
        for s in var.getShape():
            shape.append(cdm.getDimension(s).getLength())
        self.shape = list(reversed(shape))
        try:
            dtype = self.data.getDataType()
            self.dtype = np.dtype(DTYPE_MAPPINGS[dtype])
        except AttributeError:
            self.dtype = np.dtype(np.float64)

    def __getitem__(
            self, key: xarray.core.indexing.ExplicitIndexer
        ) -> np.typing.ArrayLike:
        return xarray.core.indexing.explicit_indexing_adapter(
            key,
            self.shape,
            xarray.core.indexing.IndexingSupport.BASIC,
            self._raw_indexing_method,
        )

    def _raw_indexing_method(self, key: tuple) -> np.typing.ArrayLike:
        vname = self.var.getName()

        slicebuilder = pyfimex0.SliceBuilder(self.cdm, vname, False)
        dimsizes = []
        for k, dim, dimname in zip(key, self.shape, reversed(self.var.getShape())):
            ignore_dim = dimname
            if isinstance(k, int):
                slicebuilder.setStartAndSize(dimname, k, 1)
            elif isinstance(k, slice):
                start = k.start if k.start is not None else 0
                step = k.step if k.step is not None else 1
                stop = k.stop if k.stop is not None else dim - 1
                size = (stop - start) // step + 1
                slicebuilder.setStartAndSize(dimname, start, size)
                dimsizes.append(size)
            else:
                raise TypeError(f"Unknown type of {k}: {type(k)}")

        data = self.fh.getDataSliceSB(vname, slicebuilder)
        datav = data.values()
        data_shaped = np.reshape(datav, dimsizes)
        if self.scale_offset:
            try:
                scale_factor = self.cdm.getAttribute(vname, "scale_factor").getData().values()
            except RuntimeError:
                scale_factor = 1
            try:
                add_offset = self.cdm.getAttribute(vname, "add_offset").getData().values()
            except RuntimeError:
                add_offset = 0
            data_shaped = data_shaped*scale_factor + add_offset

        return data_shaped

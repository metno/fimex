# fimex protobuf index tools

Example script for working with fimex GRIB (`.grbfp`) and NetCDF (`.ncfp`)
protobuf index files.

## Setup

```sh
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

The `_pb2.py` stubs (`cdm_pb2.py`, `grib_index_pb2.py`, `netcdf_index_pb2.py`)
are pre-generated from the fimex `.proto` files.  To re-generate them after any
schema change, install protobuf compiler (protoc) and generate the stubs with:

```sh
protoc \
  --proto_path=../../src/protobuf \
  --proto_path=../../src/io/grib/protobuf \
  --proto_path=../../src/io/netcdf/protobuf \
  --python_out=. \
  ../../src/protobuf/cdm.proto \
  ../../src/io/grib/protobuf/grib_index.proto \
  ../../src/io/netcdf/protobuf/netcdf_index.proto
```

## patch_index_files.py

Patch the source filenames stored in a fimex protobuf index (files[1:]).
Useful after moving data files to a different directory or storage location.

```
usage: patch_index_files.py [-h] [--old-prefix OLD] [--new-prefix NEW]
                             [--output OUTPUT]
                             INDEX [INDEX ...]

options:
  --old-prefix OLD  Prefix to strip from each stored filename (default: empty)
  --new-prefix NEW  Prefix to prepend after stripping (default: empty)
  --output OUTPUT   Write result to OUTPUT instead of modifying in-place
                    (only valid with a single INDEX file)
```

### Examples

Replace an absolute path prefix:

```sh
.venv/bin/python patch_index_files.py \
    --old-prefix /old/data/path/ \
    --new-prefix /new/data/path/ \
    mydata.ncfp
```

Strip a prefix (make filenames relative):

```sh
.venv/bin/python patch_index_files.py \
    --old-prefix /absolute/path/ \
    mydata.grbfp
```

Add a prefix to relative filenames:

```sh
.venv/bin/python patch_index_files.py \
    --new-prefix /absolute/path/ \
    mydata.grbfp
```

Patch multiple files in one invocation (in-place):

```sh
.venv/bin/python patch_index_files.py \
    --old-prefix /old/ --new-prefix /new/ \
    *.ncfp *.grbfp
```

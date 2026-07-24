# Installation (from VCS)

```sh
pip install "git+https://github.com/magnusuMET/fimex.git@feature/xarray#subdirectory=modules/xarray"
```

# Usage

The module is installed as an engine which `xarray` can use directly. You can open a dataset using
```python
import xarray

xarray.open_dataset("mydataset.nc", engine="fimex")
```

Pass `config` or `filetype` if necessary:
```python
import xarray

xarray.open_dataset("mydataset", filtype="grbml", config="myconfig.xml", engine="fimex")
```

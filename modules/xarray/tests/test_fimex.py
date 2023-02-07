import xarray
import os

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "..", "test")


def test_open_hirlam12():
    path = os.path.join(test_srcdir, "hirlam12.nc")
    path = os.path.join(test_srcdir, "verticalPressure.nc")
    path = os.path.join(test_srcdir, "erai.sfc.40N.0.75d.200301011200.nc")
    path = os.path.join(test_srcdir, "verticalOceanSG2.nc")
    ds = xarray.open_dataset(path, engine="fimex", decode_times=False)

    temp = ds["temp"]
    selection = temp.isel(ocean_time=3, s_rho=[4, 6, 9], xi_rho=range(4, 7))
    values = selection.values

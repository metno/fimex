import xarray
import os
import numpy as np

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


def test_isel_slice():
    path = os.path.join(test_srcdir, "verticalOceanSG2.nc")
    ds = xarray.open_dataset(path, engine="fimex", decode_times=False)

    zeta = ds["zeta"]

    zeta_sub = ds.isel(eta_rho=slice(None, None, None))["zeta"]
    assert np.all(zeta == zeta_sub)

    zeta_sub = ds.isel(eta_rho=slice(2, None, None))["zeta"]
    zeta_isel = zeta.isel(eta_rho=slice(2, None, None))
    assert np.all(zeta_isel == zeta_sub)

    zeta_sub = ds.isel(eta_rho=slice(2, 10, None))["zeta"]
    zeta_isel = zeta.isel(eta_rho=slice(2, 10, None))
    assert np.all(zeta_isel == zeta_sub)

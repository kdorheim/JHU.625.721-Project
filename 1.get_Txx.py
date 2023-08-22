from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import xarray as xr
import intake_esm as intake
import fsspec
import pkg_resources

def get_ds_meta(ds):
    """ Get the meta data information from the xarray data set.

    :param ds:  xarray dataset of CMIP data.

    :return:    pandas dataset of MIP information.
    """
    v = ds.variable_id

    data = [{'variable': v,
             'experiment': ds.experiment_id,
             'units': ds[v].attrs['units'],
             'frequency': ds.attrs["frequency"],
             'ensemble': ds.attrs["variant_label"],
             'model': ds.source_id}]
    df = pd.DataFrame(data)

    return df

def combine_df(df1, df2):
    """ Join the data frames together.

    :param df1:   pandas data frame 1.
    :param df2:   pandas data frame 2.

    :return:    a single pandas data frame.
    """
    incommon = df1.columns.intersection(df2.columns)
    if len(incommon) > 0:
        raise TypeError(f"a: df1 and df2 must have unique column names")

    # Combine the two data frames with one another.
    df1["j"] = 1
    df2["j"] = 1
    out = df1.merge(df2)
    out = out.drop(columns="j")

    return out

def get_TXx(nc_path):
    """ Process and save TXx data.

    :param nc_path:     path to the CMIP6 netcdf file available via pangeo

    :return:    path to the csv file
    """
    temp_dir = "./data"
    tag = nc_path.replace("/", "_")
    file_name = tag.replace("gs:__", "") + "TXx.csv"
    ofile = temp_dir + "/" + file_name

    nc = xr.open_zarr(fsspec.get_mapper(nc_path))
    # Select the value for the coordinates for JHU 39.3299° N, 76.6205° W
    nc = nc.sel(lat=39.32, lon=76.62, method="nearest")
    # Get the annual max value
    annual_max = nc.groupby("time.year").max()

    meta_info = get_ds_meta(annual_max)
    yr = annual_max["year"].values
    vals = annual_max["tasmax"].values
    d = {"year": yr, "value": vals}
    df = pd.DataFrame(data=d)
    out = combine_df(meta_info, df)

    # Write the output
    out.to_csv(ofile, index=False)
    return ofile


# Example of processing a single netcdf.
nc_path = "gs://cmip6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/day/tasmax/gn/v20190429/"
get_TXx("gs://cmip6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/day/tasmax/gn/v20190429/")

# -------------------------------------------------------------------------------------------
# Find the files to process.
catalog = intake.esm_datastore("https://storage.googleapis.com/cmip6/pangeo-cmip6.json")
query = dict(
    experiment_id=['historical', 'piControl', 'ssp119', 'ssp245', 'ssp585'],
    table_id='day',
    source_id = "CanESM5",
    variable_id=['tasmax'])
catalog_subset = catalog.search(require_all_on=["source_id"], **query).df
to_process = catalog_subset.loc[catalog_subset['member_id'].str.contains('p1')].zstore

# Process the CanESM files! - note that this is a temperature value... will also need to take a look
# at the pre-industrial value.
rslt_files = list(map(get_TXx, to_process))
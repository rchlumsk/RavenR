README
================

[![CRAN
status](https://www.r-pkg.org/badges/version/RavenR)](https://cran.r-project.org/package=RavenR)
[![downloads](https://cranlogs.r-pkg.org:443/badges/grand-total/RavenR?color=yellowgreen)](https://cranlogs.r-pkg.org:443/badges/grand-total/RavenR?color=yellowgreen)
![lifecycle](./man/figures/lifecycle-experimental.svg)
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4248183.svg)](https://doi.org/10.5281/zenodo.4248183)

# RavenR <img src="inst/extdata/logo/RavenR_logo_small.png" align="right" />

`RavenR` is an R package for handling [Raven](https://raven.uwaterloo.ca/) hydrologic modelling framework
inputs, outputs, and diagnostics. 

## Installation

The CRAN version of the package may also installed with:
``` r
install.packages("RavenR")
```

You can also install the latest `RavenR` version from github with:

``` r
if (!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("rchlumsk/RavenR")
```

The developmental version can be installed from the `dev` branch with:

``` r
if (!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("rchlumsk/RavenR", ref="dev")
```

## Tutorials and Quick Start Guide

Please see the package vignette for an overview of the `RavenR` package. 
Sample data is included in the package, so you need only
to install the `RavenR` library and follow along in the guide documents to
get started.

The `RavenR` vignette can be accessed with the `browseVignettes` function.
``` r
browseVignettes("RavenR")
```

## Citation (Publication)
To cite `RavenR` in publications, please use the publication reference below.

> Chlumsky, R., Craig, J. R., Lin, S. G. M., Grass, S., Scantlebury, L., Brown, G., and Arabzadeh, R.: RavenR v2.1.4: an open-source R package to support flexible hydrologic modelling, Geosci. Model Dev., 15, 7017–7030, https://doi.org/10.5194/gmd-15-7017-2022, 2022.
  
A BibTeX entry for LaTeX users is:

>  @Article{gmd-15-7017-2022,  
>      title = {\texttt{RavenR} v2.1.4: an open-source R package to support flexible hydrologic modelling},  
>      author = {Chlumsky, R. and Craig, J. R. and Lin, S. G. M. and Grass, S. and Scantlebury, L. and Brown, G. and Arabzadeh, R.},  
>      journal = {Geoscientific Model Development},  
>      volume = {15},  
>      year = {2022},  
>      number = {18},  
>      pages = {7017--7030},  
>      url = {https://gmd.copernicus.org/articles/15/7017/2022/},  
>      doi = {10.5194/gmd-15-7017-2022}  
>  }  

## Citation (Software)
```{r}
citation("RavenR")
```

The `RavenR` software itself can also be cited in publications, use:

> Robert Chlumsky, James Craig, Leland Scantlebury, Simon
  Lin, Sarah Grass, Genevieve Brown and Rezgar Arabzadeh
  (2022). RavenR: Raven Hydrological Modelling Framework R Support and Analysis. R
  package version 2.1.9. https://github.com/rchlumsk/RavenR
  
A BibTeX entry for LaTeX users is:

>  @Manual{RavenRPackage,  
>      title = {RavenR: Raven Hydrological Modelling Framework R Support and Analysis},  
>      author = {Robert Chlumsky and James Craig and Leland Scantlebury and Simon Lin and Sarah Grass and Genevieve Brown and Rezgar Arabzadeh},  
>      year = {2022},  
>      note = {R package version 2.1.9},  
>      url = {https://github.com/rchlumsk/RavenR},  
>      doi = {10.5281/zenodo.3468441}
>  }


## RavenR Wishlist

Any issues or feature requests can be submitted on the [Github Issues page](https://github.com/rchlumsk/RavenR/issues) as an issue, or 
discussed more openly on the new [Github Discussions page](https://github.com/rchlumsk/RavenR/discussions).

## Version Update Notes

### 2.2.1

Minor updates and new functionality, including:

  - new function for generic reading of output csv files, `rvn_csv_read`;
  - new function for writing Ostrich calibration templates, `rvn_rvp_calib_template`;
  - new function for updating commands in the rvi file, `rvn_rvi_commandupdate`;
  - update of all http urls to https;
  - update in rvn_rvt_met_write to move the :RedirectToFile within the gauge block; and
  - update to master database files for new connections and infilling some parameter ranges;

### 2.2.0

Minor updates and new functionality, including:

  - update to blankHRUdf and blankSBdf functions to make them slightly more intelligent;
  - update to master database files for HYPR model parameters and connections;
  - updated package vignette minor deficiency where rvp_out was not writing to a temporary file; and 
  - minor fixes to `rvn_rvh_query`, `rvn_met_recordplot` documentation.

### 2.1.8

Minor updates and new functionality, including:

  - added new functions including `rvn_rvh_query` and `rvn_rvh_summarize` for working with RVH files;
  - updated package vignette with sections on building a basic model files and a model workflow for RVI/RVP files, and a third exercise; and 
  - fix to `rvn_monthly_vbias` to exclude months with missing days;


### 2.1.7

Minor updates and bug fixes, including:

  - added new functions including `rvn_rvp_fill_template` for filling in rvp template files, `rvn_met_recordplot` to show station record lengths; 
  - added function for plotting subbasin network with interactive `visNetwork` library
  - updated `rvn_run` with additional options, including adding rvi commands to the file before execution;   
  - added new model templates from Raven Manual v3.5 to `rvn_rvi_write_template`; 
  - enhancements to `rvn_rvh_read` for splitting subbasin and HRU information in separate files, other minor improvements; and
  - bug fix in `rvn_rvt_write` for writing initial start date with correct month code;


### 2.1.4

Updates to a number of functions and new features implemented, including:

  - removal of all dependencies on spatial packages (e.g. sf, raster),
    and removal of the netcdf-related functions; 
  - `rvn_download` and `rvn_run` to enable downloading and running Raven.exe within R;    
  - `rvn_rvi_write_template` to write model rvi files from templates in the Raven manual;
  - `rvn_budyko_plot` to generate a budyko curve from model precip, AET, and PET;
  - updates to rvi mapping, including improvements to add functionality from `ggrepel` library
    in spacing labels, and addition of the `DiagrammeR` library to support `rvn_rvi_process_diagrammer`;
  - `rvn_budyko_plot` to generate a budyko curve from model precip, AET, and PET;
  - updates to handling rvt reading and writing, which is now generic for all rvt types; and
  - `rvn_met_interpolate` for performing inverse distance weighting interpolation to fill 
    missing data values in meteorological data (works immediately with `weathercan` downloads).

### 2.0.0

Major updates to the package, including core package styling such as
function case, naming conventions, and use of ggplot2 libraries for
plotting. Some of these include:

  - all core RavenR functions have an ‘rvn\_’ prefix to specify the
    package origin;  
  - functions and input/output variables now use an underscore naming
    convention, rather than periods or camelCase;  
  - ggplot2 is now the default plotting library for plotting functions,
    and some plotting arguments have been reduced, as ggplot objects may
    be modified in post-production by the user;  
  - many new functions added;  
  - new sample data sets and examples, including tidying of all function
    examples and addition of netcdf files to test netcdf-related
    functions; and  
  - multiple bug fixes and improvements to handling rvi, rvh, and
    writing rvt files.

## See Also

* [RavenR.extras](https://github.com/rchlumsk/RavenR.extras): Additional RavenR utilities.

* [Raven Hydrologic Modelling Framework](https://raven.uwaterloo.ca/): Robust and flexible Hydrologic Modelling Framework developd by Dr. James R. Craig at the University of Waterloo.

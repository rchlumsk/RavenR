
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/rchlumsk/RavenR.svg?branch=master)](https://travis-ci.org/rchlumsk/RavenR)
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3468442.svg)](https://doi.org/10.5281/zenodo.3468442)

# RavenR

RavenR is an R package for handling Raven hydrologic modelling framework
inputs, outputs, and diagnostics. Please contact Robert Chlumsky
(<rchlumsk@uwaterloo.ca>) or Dr. James Craig (<jrcraig@uwaterloo.ca>)
for any troubleshooting, bug fixes, or recommendations on future
releases.

## Installation

You can install RavenR from github with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("rchlumsk/RavenR")
```

## Tutorials and Quick Start Guide

Please see the vignettes or pdf folder for RavenR Tutorials and Quick
Start Guides. Sample data is included in the package, so you need only
to install the RavenR library and follow along in the guide documents to
get started.

\*\*Note that as of v1.2, the vignettes are out of date and will be
updated in the next update.

## RavenR Wishlist

Any issues or feature requests can be submitted on Github via the Issues
tab. You may also submit feature requests directly to Robert Chlumsky
(<rchlumsk@uwaterloo.ca>) via email.

## Dependency Installs

Note that some of the package dependencies may require the installation
of programs outside of R, particularly for Linux users. Refer to
specific function helps on how to install various package materials,
such as [ImageMagick](https://www.imagemagick.org/script/download.php).

## Version Update Notes

### 1.3

Updated functionality and test shapefile data sets for use, with a focus
on updated documentation. New functions include: \* dygraph plotting of
subbasin results  
\* RVI read in function  
\* minor fixes to flagging in hyd.read  
\* major updates to SBMap.plot and SBMap.animate to use sf and magick
packages

Sample data sets added include: \* Nith.rvi (from Raven Walkthrough
Tutorial files)  
\* Nith subwatershed shapefile (for testing GIS-based functions)  
\* additional custom outputs for run1

Updated RavenR Short Exercise with new functions, and started RavenR
Tutorial document (planned for future release, currently under
construction).

### 1.2

Updated functionality and test data sets for use. New functions include:
\* subbasin and HRU plotting from shapefile from custom data or tabular
input  
\* animation of subbasin plots for custom data  
\* watershed network plotting from HRU file  
\* RVH file handling  
\* time series infilling  
\* creation of observation rvt files

New sample data sets, including raw data for testing read-type
functions.

Updated examples for each function and improved documentation.

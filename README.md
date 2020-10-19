
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsviz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/xtreamsrl/tsviz.svg?branch=master)](https://travis-ci.org/donlelef/tsviz)
[![CRAN
status](https://www.r-pkg.org/badges/version/tsviz)](https://CRAN.R-project.org/package=tsviz)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/tsviz)](https://cran.r-project.org/package=tsviz)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/tsviz)](https://cran.r-project.org/package=tsviz)
[![Say Thanks!](https://img.shields.io/badge/Say%20Thanks-!-1EAEDB.svg)](https://saythanks.io/to/donlelef@gmail.com)
<!-- badges: end -->

<img src="man/figures/tsviz.png" align="right"/>

An RStudio addin to provide easy and interactive time series
visualization. To be visible to the addin, time series must be stored in
a dataframe in the global environment, with:

  - at least a column of type *Date*
  - at least a column of type *numeric*

## Installation

You can install the released version of tsviz from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tsviz")
```

Or install the development version from Github:

``` r
devtools::install_github("donlelef/tsviz")
```

Once you have installed the package, you do not need to load it with
`library()`, the addins are installed on your machine as part of the
package install process.

## Example

First, let us load some suitable data:

``` r
library(tsviz)
prices <- crypto_prices
```

Then, we can run the addin:

![Tutorial gif](man/figures/tsviz.gif)

That’s it.

Have a look to the demo performed at erum 2020! Watch on YouTube: https://youtu.be/t8PZbP5b8EM?t=1870.


# Who we are
<img align="left" width="80" height="80" src="https://avatars2.githubusercontent.com/u/38501645?s=450&u=1eb7348ca81f5cd27ce9c02e689f518d903852b1&v=4">
A proudly 🇮🇹 software development and data science startup.<br>We consider ourselves a family of talented and passionate people building their own products and powerful solutions for our clients. Get to know us more on <a target="_blank" href="https://xtreamers.io">xtreamers.io</a> or follow us on <a target="_blank" href="https://it.linkedin.com/company/xtream-srl">LinkedIn</a>.

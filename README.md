
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) -->
<!-- [![R build status](https://github.com/bcgov/dipr/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/dipr) -->
<!-- <a id="devex-badge" rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a> -->
<!-- badges: end -->

# dipr

The `dipr` package is an R package that loads and provides means of
caching of `.dat.gz` files. Functions are also provided to convert from
`.dat.gz` files to Apache Arrow formats.

## Installation

While this package is intended for us within the SRE you can install the
package from GitHub:

``` r
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("bcgov/dipr")
```

## Installation inside the SRE

Installation of `dipr` is slightly different than typical R package
installation. To begin with you will need to clone the dipr repo. To do
this you need access to the SRE instance of GitLab.

Go to this website: <https://projectsc.popdata.bc.ca/>

Scroll down and click on “Sign in with Pop Data Sso”

You will need to do this step everytime you log into the SRE.

Next go to this site:
<https://projectsc.popdata.bc.ca/profile/personal_access_tokens>

Create a new Personal Access Token enabling each scope and typing in
your username. This should create a new personal access token. Those are
your “git credentials”. Save those on your “U” drive along with your
user name.

### Clone from RStudio

Open RStudio and go to File -&gt; New Project -&gt; Version Control
-&gt; Git

In the field `Repository URL` section type:
<https://projectsc.popdata.bc.ca/shares/dipr>

In the field `Create project as subdirectory of` browse to where you
want to clone this repo. I’d recommend putting this in a personal but
open to other team members folder.

### Clone from bash

Navigate to where you would like to clone the repo.

    git clone https://projectsc.popdata.bc.ca/shares/dipr

### Installation using devtools

This will clone the repo and you can install from there. Inside the repo
will be an `dipr.Rproj`. Double clicking on that will open RStudio. Next
run the following code:

``` r
devtools::install()
```

Once you have the package installed you can use `dipr::dipr_update` to
update.

## Usage

The `dipr` package comes with some sample *fake* data to illustrate the
usage of the package and facilitate unit tests. Specifically, the
package provides a fake data dictionary and a compressed data file.
Example fake data can be seen like this:

``` r
library(dipr)
dipr_examples()
#> [1] "starwars-dict.nflt"  "starwars-dict.txt"   "starwars-fwf.dat"   
#> [4] "starwars-fwf.dat.gz"
```

Individual paths can be extracted like this:

``` r
data_dict_path <- dipr_example("starwars-dict.txt")
dict <- read.table(data_dict_path)
dict
#>       name start stop col_type
#> 1     name     1   21        c
#> 2   height    22   24        i
#> 3     mass    25   30        d
#> 4 has_hair    31   35        l
dat_path <- dipr_example("starwars-fwf.dat.gz")
```

The main function in the package is `read_dat()`:

``` r
raw <- read_dat(data_path = dat_path,
                data_dict = dict,
                use_cache = FALSE)
#> Reading starwars-fwf
#> Rows: 59 Columns: 4
#> -- Column specification --------------------------------------------------------
#> 
#> chr (1): name
#> dbl (2): height, mass
#> lgl (1): has_hair
#> 
#> i Use `spec()` to retrieve the full column specification for this data.
#> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(raw)
#>              name height mass has_hair
#> 1: Luke Skywalker    172   77    FALSE
#> 2:          C-3PO    167   75     TRUE
#> 3:          R2-D2     96   32     TRUE
#> 4:    Darth Vader    202  136    FALSE
#> 5:    Leia Organa    150   49    FALSE
#> 6:      Owen Lars    178  120    FALSE
```

### Data Dictionary

Data Dictionaries need to be in a data frame format that includes
‘name\`, ’start’, ‘stop’, and ‘length’ columns (in any order):

``` r
> dict
# A tibble: 64 x 4
   start  stop length name                      
   <dbl> <dbl>  <dbl> <chr>   
```

### Caching

The approach to caching is for `read_dat` to create a `.dipr` folder in
your project directory and store all cached files there. This works well
if you are working primarily alone. If, however, you are working
collaboratively, you can also set the cache data directory using the
`DIPR_CACHE_PATH` environment variable. You can set this environment
variable in your .Renviron file. Other users can do the same and then
you are will be drawing cached data from the same source.

### cache\_type

Users can specify the format of the cached files via the `cache_type`
argument in `read_dat`. The default option is currently the `fst` format
from the `fst` package because it is the fastest option in R. Support
for `parquet` is still very developmental and is not recommended for
widespread usage.

## Project Status

Under development

## License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

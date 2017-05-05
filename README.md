<!-- README.md is generated from README.Rmd. Please edit that file -->
pri
===

pri deals with image files to calculate a photosynthetic parameter called photochemical reflectance index (PRI).

You can install:

-   the latest development version from github with

``` r
# install.packages("devtools") # if not installed
devtools::install_github("KeachMurakami/pri")
```

<!-- ## Reading -->
<!-- `gasexchangeR` implements the following verb for reading LI6xxx data: -->
<!-- * `read_licor()`: read `.txt` format data obtained by LI6xxx into data frames. -->
<!--     * warning messages usually originate from parsing procedure for the file header. -->
<!-- ```{r demo_read} -->
<!-- library(gasexchangeR) -->
<!-- sample_single <- "https://raw.githubusercontent.com/KeachMurakami/gasexchangeR/master/R/LI6400.txt" -->
<!-- sample_multi <- c("https://raw.githubusercontent.com/KeachMurakami/gasexchangeR/master/R/LI6400.txt", -->
<!--                   "https://raw.githubusercontent.com/KeachMurakami/gasexchangeR/master/R/LI6400XT.txt") -->
<!-- # simple read -->
<!-- read_licor(file = sample_single) -->
<!-- # include logs for changes in conditions -->
<!-- read_licor(file = sample_single, info_log = T) -->
<!-- # read multiple files into a data frame -->
<!-- read_licor(file = sample_multi) -->
<!-- ``` -->
<!-- ## Visualizations -->
<!-- `gasexchangeR` implements the following verbs useful for visualizations: -->
<!-- * `li_scat()`: view a relationship between variables -->
<!--     * `li_scat_light()`: -->
<!--     * `li_scat_co2()`: -->
<!--     * `li_scat_h2o()`: -->
<!--     * `li_scat_temp()`: -->
<!-- * `li_course()`: view time course of a variable -->
<!-- * `li_check()`: view primitive plots to check the environmental stability during the measurement -->
<!-- ```{r demo_plot} -->
<!-- li_scat(file = sample_multi, color = "VpdL") -->
<!-- li_course(file = sample_multi, color = "Ci") -->
<!-- ``` -->
Session information
-------------------

``` r
devtools::session_info()
#> Session info --------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.3.1 (2016-06-21)
#>  system   x86_64, darwin13.4.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  tz       Asia/Tokyo                  
#>  date     2017-05-05
#> Packages ------------------------------------------------------------------
#>  package   * version  date       source                        
#>  backports   1.0.4    2016-10-24 cran (@1.0.4)                 
#>  devtools    1.12.0   2016-06-24 CRAN (R 3.3.0)                
#>  digest      0.6.11   2017-01-03 cran (@0.6.11)                
#>  evaluate    0.10     2016-10-11 cran (@0.10)                  
#>  htmltools   0.3.5    2016-03-21 CRAN (R 3.3.1)                
#>  knitr       1.15.1   2016-11-22 cran (@1.15.1)                
#>  magrittr    1.5      2014-11-22 CRAN (R 3.3.1)                
#>  memoise     1.0.0    2016-01-29 CRAN (R 3.3.1)                
#>  Rcpp        0.12.9.4 2017-03-03 Github (RcppCore/Rcpp@0566d7c)
#>  rmarkdown   1.3      2016-12-21 cran (@1.3)                   
#>  rprojroot   1.2      2017-01-16 cran (@1.2)                   
#>  stringi     1.1.2    2016-10-01 cran (@1.1.2)                 
#>  stringr     1.2.0    2017-02-18 cran (@1.2.0)                 
#>  withr       1.0.2    2016-06-20 CRAN (R 3.3.1)                
#>  yaml        2.1.14   2016-11-12 cran (@2.1.14)
```

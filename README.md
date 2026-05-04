
# simplybee-explorer

<!-- badges: start -->
<!-- badges: end -->

An interactive Shiny app for teaching the [SIMplyBee](https://www.simplybee.info) 
R package — simulating honeybee populations and breeding programmes.

## Run locally

```r
# 1. Clone the repo and open the project in RStudio, then:
install.packages("renv")   # if not already installed
renv::restore()            # installs all packages from renv.lock
shiny::runApp("app.R")     # launch the app
```

## Requirements
- R >= 4.2
- RStudio (recommended)
- All other dependencies are handled automatically by `renv`
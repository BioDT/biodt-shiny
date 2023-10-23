# BioDT Shiny Application

This repository contains a shiny application which is intended as the simplest way of interacting with BioDT by end-users.

## Modules

There are modules for each pDT.

## Technicals

🤖 This shiny app uses {golem} (https://github.com/ThinkR-open/golem & https://engineering-shiny.org/golem.html). The {golem} package is a framework for building production-grade shiny applications.

🚀 The shiny app is split into shiny modules for each pDT. This gives each pDT the freedom to design a user interface that meets their specific needs. Each module is made up of an `.R` file in `/R/` and needs to be specified as a module in `R/app_server.R`.

🔒 Authentication and access to running models on LUMI/KAROLINA is enabled using R package {r4lexis} https://github.com/It4innovations/r4lexis the package is only available on GitHub.

🎨 We use {bslib} in order to use bootstrap 4 elements (https://rstudio.github.io/bslib/). The theme is a custom BioDT theme. The css, favicons, backgrounds etc. are located in `inst/app/www`.

✅ Tests are developed using the {testthat} package. Tests are written as `.R` files in `tests/testthat/`.

🌍 Maps are rendered using {leaflet}: https://rstudio.github.io/leaflet/.

## Getting started (development set up)

We use {renv} for pacakge management, please see an introduction to {renv} here: https://rstudio.github.io/renv/articles/renv.html

Install `renv` with `install.packages("renv")`

Initalise renv with `renv::init()`

Restore the renv from the snapshot `renv::restore()` this will install all the required R packages

Launch the app `golem::run_dev()`

If you add new packages to the app requirements then update the snapshot with `renv::snapshot()`

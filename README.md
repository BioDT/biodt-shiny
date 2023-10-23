# BioDT Shiny Application
This repository contains a shiny application which is intended as the simplest way of interacting with BioDT by end-users.

## Modules

There are modules for each pDT

## Getting started (development set up)

Install `renv` with `install.packages("renv")`

Initalise renv with `renv::init()`

Restore the renv from the snapshot `renv::restore()`

Launch the app `golem::run_dev()`

Update the snapshot (if you add new packages): `renv::snapshot()`

#!/usr/bin/env Rscript

# Load all required packages
library(lubridate)
library(httr2)
library(logger)
library(fs)
library(stringr)
library(dplyr)
library(tibble)
library(jsonlite)
library(memoise)
library(terra)
library(sf)

# Create a modified environment where the modules are available
run_env <- new.env()

# Add all loaded packages to the environment with $ accessor
for (pkg in c("lubridate", "httr2", "logger", "fs", "stringr",
              "dplyr", "tibble", "jsonlite", "memoise", "terra", "sf")) {
  # Create a reference in the environment using the package name
  assign(pkg, getNamespace(pkg), envir = run_env)
}

# Add the %within% operator to the environment
run_env$`%within%` <- lubridate::`%within%`

# Set working directory
setwd("/home/artyinfact/biodt-shiny")

# Source the file in the modified environment
sys.source("app/view/rtbm/rtbm_data_handlers.R", envir = run_env)

# Now run the function from that environment
message("Starting update_local_cache...")
run_env$update_local_cache(start_date = "2025-03-14")

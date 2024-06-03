# BioDT Web Application

## Overview

This repository contains the codes for the Shiny web application hosted at [app.biodt.eu](https://app.biodt.eu). The Shiny app is intended as the simplest way of interacting with BioDT by end-users. The app uses [Shiny framework](https://shiny.posit.co/) on top of [R language ](https://www.r-project.org/) and is built using power of [development framework Rhino](https://appsilon.github.io/rhino/).

## Getting Started (development set up)

### 0. Prerequisites

On your local computer there have to be downloaded and installed these binaries and frameworks (ie. R packages):

* [Git](https://git-scm.com/downloads)
* [R language](https://cran.r-project.org/)
* [Shiny framework](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html) (see instructions below)
* [Rhino development framework](https://appsilon.github.io/rhino/#installation) (see instructions below)
* You might also want to have [Node.js installed](https://nodejs.org/en/download/package-manager) due to utilization [of the state of the art JavaScript and Sass development tools provided by Rhino](https://appsilon.github.io/rhino/articles/tutorial/create-your-first-rhino-app.html#dependencies)


### 1. Get the code

Clone the repository:

```bash
git clone git@github.com:BioDT/biodt-shiny.git
```

Open the project directory in your preferred IDE, for example ([RStudio](https://posit.co/download/rstudio-desktop/) or (VS Code)[https://code.visualstudio.com/download]).

### 2. Install required packages

Start by installing [renv](https://rstudio.github.io/renv/) package.

```R
install.packages("renv")
```

Then install all the dependencies by calling `renv::restore()` command. All the dependencies are stored in the `renv.lock` file.

```R
renv::restore()
```

### 3. Setup local development, or production, environment

We utilize a [common way for setting up your enviroment](https://appsilon.github.io/rhino/articles/how-to/manage-secrets-and-environments.html). There are two common options depending whether you want to run the app locally for development purposes (`dev`), or in production environment (`prod`), ie. for example dockerized app hosted at [app.biodt.eu](https://app.biodt.eu).

In the working directory you need to create your own `.Renviron` file which is git ignored. You can easily do it by issuing the following command in your Bash (Zsh, etc) terminal

```
cp .Renviron.example .Renviron
```

**Config env variable**

In the file please config what enviroment you want the app run at. For **development**:

```bash
(...)
R_CONFIG_ACTIVE="dev"
(...)
```

For **production** delete the line `R_CONFIG_ACTIVE="dev"` and **uncomment this line** which results in:

```bash
(...)
R_CONFIG_ACTIVE="prod"
(...)
```

Other environment variables, which aren't secrets (ie. git ignored) and are publicily avaible, can be seen and/or edit in the file `config.yml`. At the time (May 2024) the file contains dummy variables, serving as an example only.

Note! **You might probably want to restart (re-open) your R terminal at this moment and restart your R session**.

### 4. Get local data

Download any required local data, first you need to create a folder to hold this data. This folder is ignored by git so you need to create it first. You can do this manually or run in R

```
dir.create("app/data")
```

Then download the data from the
[sharepoint](https://tt.eduuni.fi/sites/csc-rdi-fileshare/BioDT/Forms/AllItems.aspx?RootFolder=%2Fsites%2Fcsc%2Drdi%2Dfileshare%2FBioDT%2FWP7%20%2D%20Integration%20%26%20Service%20Uptake%20with%20Research%20Infrastructure%20Environments%2FShinyAppData) (authenticated access required)

Each pDT's shiny module has it's own folder for local data within this which you can see specififed in this file: https://github.com/BioDT/biodt-shiny/blob/main/dev/run_dev.R therefore you need to create a folder within the `local_data` folder. The folder names are:

 - Crop wild relatives: `app/data/cwr`
 - BEEHAVE: `app/data/honeybee`
 - Cultural ecosystem services: `app/data/ces` 

### 5. Launch the app

Now you should be ready to launch the app, which you can do using this command in your R terminal.

```R
shiny::runApp()
```

### Development

Please feel free to create a branch and pull requests for making significant changes to the Shiny app.

## Modules

The app is modularized and each pDT have files in its own subfolder. UI files are located mainly in the `app/view` subfolder, R function mainly in the `app/logic` subfolder. UI files for each pDT is located:

 * BEEHAVE: `app/view/honeybee`
 * Cultural Ecosystem Services: `TBD`
 * Crop wild relatives: `TBD`
 * GRASSLAND: `app/view/grassland`
 * Invasive alien species: `TBD`

## Technicals

🔒 Authentication and access to running models on LUMI/KAROLINA is enabled using R package {r4lexis} https://github.com/It4innovations/r4lexis the package is only available on GitHub.

🎨 We use {bslib} in order to use Bootstrap 5 elements (https://rstudio.github.io/bslib/). The theme is a custom BioDT theme. The css, favicons, backgrounds etc. are located in `inst/app/www`.

✅ Tests are developed using the {testthat} package. Tests are written as `.R` files in `tests/testthat/`.

🌍 Maps are rendered using {leaflet}: https://rstudio.github.io/leaflet/.

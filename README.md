# BioDT Web Application

## Overview

This repository contains the codes for the Shiny web application hosted at [app.biodt.eu](https://app.biodt.eu). The Shiny app is intended as the simplest way of interacting with BioDT by end-users. The app uses [Shiny framework](https://shiny.posit.co/) on top of [R language ](https://www.r-project.org/) and is built using power of [development framework Rhino](https://appsilon.github.io/rhino/).

## Quick Start with Docker Compose

The easiest way to run the BioDT Shiny app is using Docker Compose:

### Prerequisites

* [Docker](https://docs.docker.com/get-docker/)
* [Docker Compose](https://docs.docker.com/compose/install/)

### Running the app

1. Clone the repository:

```bash
git clone git@github.com:BioDT/biodt-shiny.git
cd biodt-shiny
```

2. Setup local data (see [section 4](#4-get-local-data) for details on which pDTs require data)

   **Note:** By default, the `docker-compose.yml` expects data in `${PWD}/app/data`. If you place your data in a different location, update the left-hand side of the volume mount in `docker-compose.yml`:
   
   ```yaml
   volumes:
     - "/your/custom/path:/app/data"  # Change /your/custom/path to your data location
   ```

3. Start the application:

```bash
docker compose up
```

4. Open your browser and navigate to `http://localhost:7860`

To stop the application, press `Ctrl+C` or run:

```bash
docker compose down
```

**Note:** The Honeybee and Disease Outbreaks pDTs require Docker socket access (`/var/run/docker.sock`) which is already configured in the `docker-compose.yml` file.

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

**Config .Rprofile**

We prefer to use PPM packages where possible and therefore it is advised to use following lines in the `.Rprofile` file in the home directory of the project.

```r
source("renv/activate.R")

options(repos = c(PPM = "https://packagemanager.posit.co/cran/latest"))
```

In case of Windows and MacOS environment You might want to add one more line after these two:

```r
options(pkgType = "binary")
```

Note! **You might probably want to restart (re-open) your R terminal at this moment and restart your R session**.

### 4. Get local data

Not all pDTs require local data, and data is only needed when accessing specific pDT pages. Therefore, you typically don't need to download all data for development.

**pDTs that don't require local data:**
- Invasive Alien Species (data is embedded)
- Real-time Bird Monitoring (data is fetched automatically)

**For pDTs that do require local data:**

First, you need to create a folder to hold this data. This folder is ignored by git so you need to create it first. You can do this manually or run in R

```
dir.create("app/data")
```

If you need access to data for development, please contact tomas.martinovic@vsb.cz or reach out through LifeWatch ERIC channels.

Each pDT's shiny module has its own folder for local data within this which you can see specified in the `config.yml` file. The folder names are:

 - Crop wild relatives: `app/data/cwr`
 - BEEHAVE: `app/data/honeybee`
 - Cultural ecosystem services: `app/data/ces`
 - Disease outbreaks: `app/data/disease_outbreak`
 - Forest biodiversity: `app/data/forest_bird`
 - Grassland dynamics: `app/data/grassland`
 - Real-time bird monitoring: `app/data/rtbm` 

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
 * Cultural Ecosystem Services: `app/view/ces`
 * Crop wild relatives: `app/view/cwr`
 * Disease outbreaks: `app/view/disease_outbreaks`
 * Forest biodiversity: `app/view/forest`
 * GRASSLAND: `app/view/grassland`
 * Invasive alien species: `app/view/ias`
 * RTBM (Real-time Bird Monitoring): `app/view/rtbm`

## Technicals

🎨 We use {bslib} in order to use Bootstrap 5 elements (https://rstudio.github.io/bslib/). The theme is a custom BioDT theme. The css, favicons, backgrounds etc. are located in `inst/app/www`.

✅ Tests are developed using the {testthat} package. Tests are written as `.R` files in `tests/testthat/`.

🌍 Maps are rendered using {leaflet}: https://rstudio.github.io/leaflet/.

### Loading screens with {waiter}

If you have computations that take a long time then use the implemented waiter functionality. This will not make the app load faster but make it feel faster as it induces patience in your users and make the app feel slicker. To set this up in your module you can use `waiter_text()` function to prepare text message with custom HTML format.

```r
    msg <- 
      waiter_text(message = tags$h3("Computing Beehave simulation...",
        style = "color: #414f2f;"
      ))
```

Then create a waiter object

```r
w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )
``` 

You can then use the following lines within your shiny code:

 * `w$show()` to show a loading screen
 * `w$update()` to update message in the middle of computation
 * `w$hide()` to hide a loading screen

See https://waiter.john-coene.com/ for more info


# BioDT Shiny Application

This repository contains a shiny application which is intended as the simplest way of interacting with BioDT by end-users. The shiny app is built using the golem framework.

[Here is a link to the figma for designing the GUI (graphical user interface).](https://www.figma.com/file/92WkNhlVG1nWI2bxBgoXzE/GUI?type=design&mode=design)

## Code style preferences

Please style the files according to `grkstyle::grk_style_transformer()`

```{r}
# If you work in RStudio
options(styler.addins_style_transformer = "grkstyle::grk_style_transformer()")

# If you work in VSCode
options(languageserver.formatting_style = function(options) {
  grkstyle::grk_style_transformer()
})
```

## Modules

There are modules for each pDT:

 * BEEHAVE: `R/mod_BEEHAVE.R`
 * Cultural Ecosystem Services: `R/mod_cultural_ecosystem_services.R`
 * Crop wild relatives: `R/mod_cwr.R`
 * GRASSLAND: `R/mod_grassland`
 * Invasive alien species: `R/mod_ias.R`

## Technicals

🤖 This shiny app uses {golem} (https://github.com/ThinkR-open/golem & https://engineering-shiny.org/golem.html). The {golem} package is a framework for building production-grade shiny applications.

🚀 The shiny app is split into shiny modules for each pDT. This gives each pDT the freedom to design a user interface that meets their specific needs. Each module is made up of an `.R` file in `/R/` and needs to be specified as a module in `R/app_server.R`.

🔒 Authentication and access to running models on LUMI/KAROLINA is enabled using R package {r4lexis} https://github.com/It4innovations/r4lexis the package is only available on GitHub.

🎨 We use {bslib} in order to use bootstrap 4 elements (https://rstudio.github.io/bslib/). The theme is a custom BioDT theme. The css, favicons, backgrounds etc. are located in `inst/app/www`.

✅ Tests are developed using the {testthat} package. Tests are written as `.R` files in `tests/testthat/`.

🌍 Maps are rendered using {leaflet}: https://rstudio.github.io/leaflet/.

### Loading screens with {waiter}

If you have computations that take a long time then use the implemented waiter functionality. This will not make the app load faster but make it feel faster as it induces patience in your users and make the app feel slicker. To set this up go to `R/app_server.R` and ensure that the loaders are passed as an argument to your module. For example:

```mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1",r,loaders)```

Then in your module's server ensure that loaders is an argument: 

```mod_cultural_ecosystem_services_server <- function(id, r,loaders) {``` 

You can then use the following lines within your shiny code:

 * `loaders$waiter$show()` to show a loading screen
 * `loaders$waiter$hide()` to hide a loading screen
 * `loaders$hostess$set(x)` to update the progress % in the loading screen (x between 0 and 100)

See https://waiter.john-coene.com/ for more info

### Tutorials with {cicerone}

We use cicerone to create guided tours of your shiny module to help users understand how to use the app. See https://cicerone.john-coene.com/ for more info

## Getting started (development set up)

### Get the code

Clone the repository:

```
git clone git@github.com:BioDT/biodt-shiny.git
```

Open the project directory in your preferred IDE (Rstudio, VS Code)

### Install required packages

Install {renv} for package management, please see an introduction to {renv} here: https://rstudio.github.io/renv/articles/renv.html Install `renv` and initialise the environment

```
install.packages("renv")
renv::init()
```

Restore the renv from the snapshot to install all the required R packages

```
renv::restore()
```

### Get local data

Download any required local data, first you need to create a folder to hold this data. This folder is ignored by git so you need to create it first. You can do this manually or run in R

```
dir.create("local_data")
```

Then download the data from the
[sharepoint](https://tt.eduuni.fi/sites/csc-rdi-fileshare/BioDT/Forms/AllItems.aspx?RootFolder=%2Fsites%2Fcsc%2Drdi%2Dfileshare%2FBioDT%2FWP7%20%2D%20Integration%20%26%20Service%20Uptake%20with%20Research%20Infrastructure%20Environments%2FShinyAppData) (authenticated access required)

Each pDT's shiny module has it's own folder for local data within this which you can see specififed in this file: https://github.com/BioDT/biodt-shiny/blob/main/dev/run_dev.R therefore you need to create a folder within the `local_data` folder. The folder names are:

 - Crop wild relatives: `local_data/cwr`
 - BEEHAVE: `local_data/pollinators`
 - Cultural ecosystem services: `local_data/ces` 

### Launch the app

Now you should be ready to launch the app, which you can do using this command.

```
golem::run_dev()
```

### Development

Please feel free to create a branch and pull requests for making significant changes to the Shiny app. If you add new packages to the app requirements then update the snapshot with `renv::snapshot()`.

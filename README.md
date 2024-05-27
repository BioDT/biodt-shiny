# BioDT Web Application

## Overview

This repository contains the codes for the Shiny web application hosted at [app.biodt.eu](https://app.biodt.eu). The Shiny app is intended as the simplest way of interacting with BioDT by end-users. The app uses [Shiny framework](https://shiny.posit.co/) on top of [R language ](https://www.r-project.org/) and is built using power of newer [development framework Rhino](https://appsilon.github.io/rhino/).

## UI Design

Here is a link to the [Figma](https://www.figma.com/) for [designing the GUI (graphical user interface)](https://www.figma.com/file/92WkNhlVG1nWI2bxBgoXzE/GUI?type=design&mode=design) of our BioDT app.

## Getting Started (development set up)

### Prerequisites

On your local computer there have to be downloaded and installed these binaries and frameworks:

* [R language](https://cran.r-project.org/).
* [Shiny framework](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html).
* [Rhino development framework](https://appsilon.github.io/rhino/#installation).


### Get the Code

Clone the repository:

```
git clone git@github.com:BioDT/biodt-shiny.git
```

Open the project directory in your preferred IDE ([RStudio](https://posit.co/download/rstudio-desktop/), (VS Code)[https://code.visualstudio.com/download]).

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


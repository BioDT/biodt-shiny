# Load the libraries
box::use(
  # ggplot2,
  dplyr,
  # tidyr,
  terra[rast, `values<-`, values, project, flip],
  stringr,
  utils,
  shiny,
  echarty
)

#' @export
convert_landis_output <- function(r_in) {
  ## the x and y extent of the output Landis rasters
  helper_raster <- rast(
    nrows = 1155,
    ncols = 4441,
    crs = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    extent = c(116000, 560100, 6604704, 6720204)
  )

  # transfer the values of the input raster into the new raster
  values(helper_raster) <- values(r_in)

  r_wgs84 <- helper_raster |>
    flip() |>
    project("EPSG:4326")

  # Replace 0 values with NA
  r_wgs84[r_wgs84 == 0] <- NA

  return(r_wgs84)
}

#' @export
get_data <- function(
  climate_scenarios,
  management_scenarios,
  years,
  data_folder
) {
  # Create an empty list to store the data
  all_data <- list()

  # Loop through each combination of climate and management scenarios
  for (climate in climate_scenarios) {
    for (management in management_scenarios) {
      # zip_file <- paste0(data_folder, "/run_landis_", paste0(climate, "_", management), "_7141504")
      file_inside_zip <- file.path(data_folder, paste0("run_landis_", paste0(climate, "_", management), "_7141504/output/", "TotalCohorts.txt"))

      # get simulation start year
      lines <- readLines(file.path(data_folder, paste0("run_landis_", paste0(climate, "_", management), "_7141504"), "PnET-succession.txt"))
      start_year_line <- grep("^StartYear", lines, value = TRUE)
      start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))

      # Read the data if the file exists
      if (file.exists(file_inside_zip)) {
        data <- utils$read.csv(file_inside_zip)
        data$Climate <- climate
        data$Management <- management
        data$Time <- data$Time + start_year
        all_data[[paste(climate, management, sep = "_")]] <- data
      }
    }
  }
  
  # Combine all data into a single data frame
  combined_data <- dplyr$bind_rows(all_data)
  return(combined_data)
}

#' @export
get_figure <- function(
    combined_data
) {

  chart <- echarty$ec.init()
  chart$x$opts <- list(
    legend = list(
      data = c('4.5', '8.5', 'current'),
      top = '1%'
    ),
    title = list(
      list(left = '8%',
           top = '5%',
           text = 'BAU',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '22%',
           top = '5%',
           text = 'EXT10',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '36%',
           top = '5%',
           text = 'EXT30',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '50%',
           top = '5%',
           text = 'GTR30',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '64%',
           top = '5%',
           text = 'NTLR',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '78%',
           top = '5%',
           text = 'NTSR',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '92%',
           top = '5%',
           text = 'SA',
           textStyle = list(
             fontSize = 14
           ))
    ),
    xAxis = list(
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 0
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 1
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 2
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 3
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 4
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 5
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 6
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 7
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 8
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 9
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 10
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 11
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 12
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 13
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 14
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 15
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 16
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 17
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 18
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 19
      ),
      list(
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 20
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 21
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 22
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 23
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 24
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 25
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 26
      ),
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "Time"]),
        gridIndex = 27
      )
    ),
    yAxis = list(
      list(
        name = "AGBiomass (g/m^2)",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 12,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 0
      ),
      list(
        type = "value",
        gridIndex = 1
      ),
      list(
        type = "value",
        gridIndex = 2
      ),
      list(
        type = "value",
        gridIndex = 3
      ),
      list(
        type = "value",
        gridIndex = 4
      ),
      list(
        type = "value",
        gridIndex = 5
      ),
      list(
        type = "value",
        gridIndex = 6
      ),
      list(
        name = "BGBiomass (g/m^2)",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 12,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 7
      ),
      list(
        type = "value",
        gridIndex = 8
      ),
      list(
        type = "value",
        gridIndex = 9
      ),
      list(
        type = "value",
        gridIndex = 10
      ),
      list(
        type = "value",
        gridIndex = 11
      ),
      list(
        type = "value",
        gridIndex = 12
      ),
      list(
        type = "value",
        gridIndex = 13
      ),
      list(
        name = "Age (years)",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 12,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 14
      ),
      list(
        type = "value",
        gridIndex = 15
      ),
      list(
        type = "value",
        gridIndex = 16
      ),
      list(
        type = "value",
        gridIndex = 17
      ),
      list(
        type = "value",
        gridIndex = 18
      ),
      list(
        type = "value",
        gridIndex = 19
      ),
      list(
        type = "value",
        gridIndex = 20
      ),
      list(
        name = "Woody Debris (kgDW/m^2)",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 12,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 21
      ),
      list(
        type = "value",
        gridIndex = 22
      ),
      list(
        type = "value",
        gridIndex = 23
      ),
      list(
        type = "value",
        gridIndex = 24
      ),
      list(
        type = "value",
        gridIndex = 25
      ),
      list(
        type = "value",
        gridIndex = 26
      ),
      list(
        type = "value",
        gridIndex = 27
      )
    ),
    grid = list(
      list(left = '4%', top = '10%', width = '10%', height= '12%'),
      list(left = '18%', top = '10%', width = '10%', height= '12%'),
      list(left = '32%', top = '10%', width = '10%', height= '12%'),
      list(left = '46%', top = '10%', width = '10%', height= '12%'),
      list(left = '60%', top = '10%', width = '10%', height= '12%'),
      list(left = '74%', top = '10%', width = '10%', height= '12%'),
      list(left = '88%', top = '10%', width = '10%', height= '12%'),
      
      list(left = '4%', top = '31%', width = '10%', height= '12%'),
      list(left = '18%', top = '31%', width = '10%', height= '12%'),
      list(left = '32%', top = '31%', width = '10%', height= '12%'),
      list(left = '46%', top = '31%', width = '10%', height= '12%'),
      list(left = '60%', top = '31%', width = '10%', height= '12%'),
      list(left = '74%', top = '31%', width = '10%', height= '12%'),
      list(left = '88%', top = '31%', width = '10%', height= '12%'),
      
      list(left = '4%', top = '56%', width = '10%', height= '12%'),
      list(left = '18%', top = '56%', width = '10%', height= '12%'),
      list(left = '32%', top = '56%', width = '10%', height= '12%'),
      list(left = '46%', top = '56%', width = '10%', height= '12%'),
      list(left = '60%', top = '56%', width = '10%', height= '12%'),
      list(left = '74%', top = '56%', width = '10%', height= '12%'),
      list(left = '88%', top = '56%', width = '10%', height= '12%'),
      
      list(left = '4%', top = '81%', width = '10%', height= '12%'),
      list(left = '18%', top = '81%', width = '10%', height= '12%'),
      list(left = '32%', top = '81%', width = '10%', height= '12%'),
      list(left = '46%', top = '81%', width = '10%', height= '12%'),
      list(left = '60%', top = '81%', width = '10%', height= '12%'),
      list(left = '74%', top = '81%', width = '10%', height= '12%'),
      list(left = '88%', top = '81%', width = '10%', height= '12%')
      
    ),
    series = list(
      list(
        name = "4.5",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 0,
        yAxisIndex = 0,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 1,
        yAxisIndex = 1,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 2,
        yAxisIndex = 2,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 3,
        yAxisIndex = 3,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 4,
        yAxisIndex = 4,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 5,
        yAxisIndex = 5,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "4.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 6,
        yAxisIndex = 6,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 7,
        yAxisIndex = 7,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 8,
        yAxisIndex = 8,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 9,
        yAxisIndex = 9,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 10,
        yAxisIndex = 10,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 11,
        yAxisIndex = 11,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 12,
        yAxisIndex = 12,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "4.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 13,
        yAxisIndex = 13,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 14,
        yAxisIndex = 14,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 15,
        yAxisIndex = 15,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 16,
        yAxisIndex = 16,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 17,
        yAxisIndex = 17,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 18,
        yAxisIndex = 18,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 19,
        yAxisIndex = 19,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "4.5", "AverageAge"],
        type = "line",
        xAxisIndex = 20,
        yAxisIndex = 20,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 21,
        yAxisIndex = 21,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 22,
        yAxisIndex = 22,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 23,
        yAxisIndex = 23,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 24,
        yAxisIndex = 24,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 25,
        yAxisIndex = 25,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 26,
        yAxisIndex = 26,
        smooth = TRUE,
        color = "red"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "4.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 27,
        yAxisIndex = 27,
        smooth = TRUE,
        color = "red"
      ),
      ####
      list(
        name = "8.5",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 0,
        yAxisIndex = 0,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 1,
        yAxisIndex = 1,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 2,
        yAxisIndex = 2,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 3,
        yAxisIndex = 3,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 4,
        yAxisIndex = 4,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 5,
        yAxisIndex = 5,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "8.5", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 6,
        yAxisIndex = 6,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 7,
        yAxisIndex = 7,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 8,
        yAxisIndex = 8,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 9,
        yAxisIndex = 9,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 10,
        yAxisIndex = 10,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 11,
        yAxisIndex = 11,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 12,
        yAxisIndex = 12,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "8.5", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 13,
        yAxisIndex = 13,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 14,
        yAxisIndex = 14,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 15,
        yAxisIndex = 15,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 16,
        yAxisIndex = 16,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 17,
        yAxisIndex = 17,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 18,
        yAxisIndex = 18,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 19,
        yAxisIndex = 19,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "8.5", "AverageAge"],
        type = "line",
        xAxisIndex = 20,
        yAxisIndex = 20,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 21,
        yAxisIndex = 21,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 22,
        yAxisIndex = 22,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 23,
        yAxisIndex = 23,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 24,
        yAxisIndex = 24,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 25,
        yAxisIndex = 25,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 26,
        yAxisIndex = 26,
        smooth = TRUE,
        color = "green"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "8.5", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 27,
        yAxisIndex = 27,
        smooth = TRUE,
        color = "green"
      ),
      ###
      list(
        name = "current",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 0,
        yAxisIndex = 0,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 1,
        yAxisIndex = 1,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 2,
        yAxisIndex = 2,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 3,
        yAxisIndex = 3,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 4,
        yAxisIndex = 4,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 5,
        yAxisIndex = 5,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "current", "AverageB.g.m2."],
        type = "line",
        xAxisIndex = 6,
        yAxisIndex = 6,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 7,
        yAxisIndex = 7,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 8,
        yAxisIndex = 8,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 9,
        yAxisIndex = 9,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 10,
        yAxisIndex = 10,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 11,
        yAxisIndex = 11,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 12,
        yAxisIndex = 12,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "current", "AverageBelowGround.g.m2."],
        type = "line",
        xAxisIndex = 13,
        yAxisIndex = 13,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 14,
        yAxisIndex = 14,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 15,
        yAxisIndex = 15,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 16,
        yAxisIndex = 16,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 17,
        yAxisIndex = 17,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 18,
        yAxisIndex = 18,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 19,
        yAxisIndex = 19,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "current", "AverageAge"],
        type = "line",
        xAxisIndex = 20,
        yAxisIndex = 20,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "BAU",
        data = combined_data[combined_data$Management == "BAU" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 21,
        yAxisIndex = 21,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT10",
        data = combined_data[combined_data$Management == "EXT10" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 22,
        yAxisIndex = 22,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "EXT30",
        data = combined_data[combined_data$Management == "EXT30" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 23,
        yAxisIndex = 23,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "GTR30",
        data = combined_data[combined_data$Management == "GTR30" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 24,
        yAxisIndex = 24,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTLR",
        data = combined_data[combined_data$Management == "NTLR" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 25,
        yAxisIndex = 25,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "NTSR",
        data = combined_data[combined_data$Management == "NTSR" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 26,
        yAxisIndex = 26,
        smooth = TRUE,
        color = "blue"
      ),
      list(
        # name = "SA",
        data = combined_data[combined_data$Management == "SA" & combined_data$Climate == "current", "WoodyDebris.kgDW.m2."],
        type = "line",
        xAxisIndex = 27,
        yAxisIndex = 27,
        smooth = TRUE,
        color = "blue"
      )
    )
  )

  return(chart)
}

######################

#' @export
get_file_list <- function(
  input,
  data_folder,
  experiment_data
) {

  if (length(experiment_data) == 0) {
    shiny$showNotification("No files found matching the specified structure.", type = "error")
  } else if (length(experiment_data) > 1) {
    shiny$showNotification("Multiple files found matching the specified structure.", type = "error")
  } else if (length(experiment_data) != 1) {
    return(NULL)
  }

  experiment <- stringr$str_replace(
    string = experiment_data,
    pattern = data_folder,
    replacement = ""
  )

  if (stringr$str_starts(experiment, "/")) {
    experiment <- stringr$str_replace(experiment, "/", "")
  }

  data_file_list <- list.files(path = experiment_data, recursive = TRUE)
  
  # get simulated years
  lines <- readLines(file.path(experiment_data, "PnET-succession.txt"))
  start_year_line <- grep("^StartYear", lines, value = TRUE)
  start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))
  timestep_line <- grep("^Timestep", lines, value = TRUE)
  timestep <- as.numeric(sub(".*?(\\d+).*", "\\1", timestep_line))
  lines <- readLines(file.path(experiment_data, "scenario.txt"))
  duration_line <- grep("^Duration", lines, value = TRUE)
  duration <- as.numeric(sub(".*?(\\d+).*", "\\1", duration_line))
  
  if (input$output == "Above-ground biomass") {
    # todo implement update in case of "all species"
    if (input$species == "All species") {
      type <- "betulaSP"
    }
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/agbiomass/", type)
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/agbiomass/", type, "/AGBiomass[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
    
  } else if (input$output == "Below-ground biomass") {
    res_folder <- paste0(experiment, "/output/BelowGroundBiom/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/BelowGroundBiom/", "BGB[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Harvested biomass") {
    res_folder <- paste0(experiment, "/output/harvest/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/harvest/", "biomass-removed-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if (input$output == "Woody Debris") {
    res_folder <- paste0(experiment, "/output/WoodyDebris/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/WoodyDebris/", "WoodyDebris-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Max-age of selected species") {
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "All species") {
      type <- "AllSppMaxAge"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/max-age-selected-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/max-age-selected-spp/", type, "-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Average age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/age-all-spp/", "AGE-AVG-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Median age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/age-all-spp/", "AGE-MED-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  }

  return(list(
    # experiment_data_file = experiment_data_file,
    res_working_folder = res_working_folder,
    res_file_list_tick = res_file_list_tick,
    res_folder = res_folder,
    start_year = start_year,
    timestep = timestep,
    duration = duration
  ))
}

#' @export
get_experiment_data_file <- function(
    input,
    data_folder
) {
  if (input$climate == "Current climate") {
    climate <- "current"
  } else if (input$climate == "RCP4.5") {
    climate <- "4.5"
  } else if (input$climate == "RCP8.5") {
    climate <- "8.5"
  }
  
  # Scan for files with the specified structure
  pattern <- paste0("^.+_", climate, "_", input$management, "_.+\\$")
  experiment_data <- list.dirs(path = data_folder, full.names = TRUE, recursive = FALSE)
  experiment_data <- experiment_data[grepl(paste0("_", climate, "_", input$management, "_"), experiment_data)]
  
  experiment_data_file <- experiment_data
  
  return(experiment_data_file)
}


#' @export
get_file_name <- function(input, res_folder, tick) {
  if (input$output == "Above-ground biomass") {
    res_file <- paste0(res_folder, "/AGBiomass", tick, ".tif")
  } else if (input$output == "Below-ground biomass") {
    res_file <- paste0(res_folder, "BGB", tick, ".tif")
  } else if (input$output == "Harvested biomass") {
    res_file <- paste0(res_folder, "biomass-removed-", tick, ".tif")
  } else if (input$output == "Woody Debris") {
    res_file <- paste0(res_folder, "WoodyDebris-", tick, ".tif")
  } else if (input$output == "Max-age of selected species") {
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "All species") {
      type <- "AllSppMaxAge"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }

    res_file <- paste0(res_folder, type, "-", tick, ".tif")
  } else if (input$output == "Average age of all trees") {
    res_file <- paste0(res_folder, "AGE-AVG-", tick, ".tif")
  } else if (input$output == "Median age of all trees") {
    res_file <- paste0(res_folder, "AGE-MED-", tick, ".tif")
  }

  return(res_file)
}


#' @export
get_multichart <- function(
    experiment_data_file
) 
{
  
  # get data to plot
  
  all_data <- list()
  
  file_inside_zip <- file.path(experiment_data_file, "output", "TotalCohorts.txt")
  
  # get simulation start year
  lines <- readLines(file.path(experiment_data_file, "PnET-succession.txt"))
  start_year_line <- grep("^StartYear", lines, value = TRUE)
  start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))
  
  # Read the data if the file exists
  # if (file.exists(file_inside_zip)) {
  data <- utils$read.csv(file_inside_zip)
  
  data$Time <- data$Time + start_year
  chart <- echarty$ec.init()
  chart$x$opts <- list(
    title = list(
      list(
        left = '8%',
        top = '1%',
        text = 'Average age over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '30%',
        top = '1%',
        text = 'Average above-ground biomass over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '56%',
        top = '1%',
        text = 'Woody debris over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '78%',
        top = '1%',
        text = 'Average below-ground biomass over time',
        textStyle = list(
          fontSize = 14
        )
      )
    ),
    grid = list(
      list(left = '4%', top = '10%', width = '20%'),
      list(left = '28%', top = '10%', width = '20%'),
      list(left = '52%', top = '10%', width = '20%'),
      list(left = '76%', top = '10%', width = '20%')
    ),
    xAxis = list(
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 0
        ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 1
      ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 2
      ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 3
      )
    ),
    yAxis = list(
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 0
        ),
      list(
        name = "g/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 1
      ),
      list(
        name = "kgDW/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 2
      ),
      list(
        name = "g/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 3
      )
    ),
    tooltip = list(
      trigger = 'axis'
    ),
    # legend = list(
    #   data = c('Average age over time', 'Average above-ground biomass over time', 'Woody debris over time', 'Average below-ground biomass over time')
    # ),
    series = list(
      list(
        name = "Average age over time",
        data = data$AverageAge,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 0,
        yAxisIndex = 0
      ),
      list(
        name = "Average above-ground biomass over time",
        data = data$AverageB.g.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 1,
        yAxisIndex = 1
      ),
      list(
        name = "Woody debris over time",
        data = data$WoodyDebris.kgDW.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 2,
        yAxisIndex = 2
      ),
      list(
        name = "Average below-ground biomass over time",
        data = data$AverageBelowGround.g.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 3,
        yAxisIndex = 3
      )
      
    )
  )
  
  return(chart)
  
}

# #' @export
# get_multichart <- function(
#     experiment_data_file
#     ) 
#   {
#   
#   # get data to plot
#   
#   all_data <- list()
# 
#   file_inside_zip <- file.path(experiment_data_file, "output", "TotalCohorts.txt")
# 
#   # get simulation start year
#   lines <- readLines(file.path(experiment_data_file, "PnET-succession.txt"))
#   start_year_line <- grep("^StartYear", lines, value = TRUE)
#   start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))
# 
#   # Read the data if the file exists
#   # if (file.exists(file_inside_zip)) {
#     data <- utils$read.csv(file_inside_zip)
# 
#     data$Time <- data$Time + start_year
#     chart <- echarty$ec.init()
#     chart$x$opts <- list(
#       xAxis = list(
#         type = "category",
#         data = as.character(data$Time)
#       ),
#       yAxis = list(
#         type = "value"
#       ),
#       tooltip = list(
#         trigger = 'axis'
#       ),
#       legend = list(
#         data = c('Average age over time', 'Average above-ground biomass over time', 'Woody debris over time', 'Average below-ground biomass over time')
#       ),
#       series = list(
#         list(
#           name = "Average age over time",
#           data = data$AverageAge,
#           type = "line",
#           smooth = TRUE
#         ),
#         list(
#           name = "Average above-ground biomass over time",
#           data = data$AverageB.g.m2.,
#           type = "line",
#           smooth = TRUE
#         ),
#         list(
#           name = "Woody debris over time",
#           data = data$WoodyDebris.kgDW.m2.,
#           type = "line",
#           smooth = TRUE
#         ),
#         list(
#           name = "Average below-ground biomass over time",
#           data = data$AverageBelowGround.g.m2.,
#           type = "line",
#           smooth = TRUE
#         )
# 
#       )
#     )
# 
#   return(chart)
#   
# }



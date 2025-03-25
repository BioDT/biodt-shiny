# Load minimal required packages
library(jsonlite)
library(dplyr)
library(arrow)

message("Starting very basic preprocessing...")

# Base directory for data storage
base_dir <- "/home/artyinfact/biodt-shiny/app/data/rtbm"
parquet_dir <- file.path(base_dir, "parquet")

# Create directory if it doesn't exist
dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

# Create a sample dataframe with bird observation data
message("Creating sample data...")

# Load bird info from JSON to get species names
local_path <- file.path(base_dir, "bird_info.json")
if (file.exists(local_path)) {
  bird_info <- fromJSON(local_path)
} else {
  message("Bird info file not found, creating dummy data")
  bird_info <- list()
}

# Process the nested JSON structure
bird_df <- if (length(bird_info) > 0) {
  do.call(rbind, lapply(names(bird_info), function(species_key) {
    info <- bird_info[[species_key]]
    data.frame(
      common_name = info$common_name,
      scientific_name = info$scientific_name,
      stringsAsFactors = FALSE
    )
  }))
} else {
  data.frame(
    common_name = c("Arctic Loon", "Common Goldeneye"),
    scientific_name = c("Gavia_arctica", "Bucephala_clangula"),
    stringsAsFactors = FALSE
  )
}

# Create sample data for a few species and dates
sample_data <- data.frame()

for (i in 1:nrow(bird_df)) {
  species <- gsub(" ", "_", bird_df$scientific_name[i])

  # Create species directory
  species_dir <- file.path(parquet_dir, paste0("species=", species))
  dir.create(species_dir, recursive = TRUE, showWarnings = FALSE)

  # Create sample data for 14 days
  for (day in 1:14) {
    date_str <- format(Sys.Date() - day, "%Y-%m-%d")

    # Generate sample observations
    n_obs <- sample(10:100, 1)

    df <- data.frame(
      date = as.character(date_str), # Store as character to avoid date formatting issues
      scientific_name = species,
      longitude = runif(n_obs, 20, 30), # Finland longitude range
      latitude = runif(n_obs, 60, 70), # Finland latitude range
      intensity = runif(n_obs, 0, 1)
    )

    # Save to parquet file
    output_file <- file.path(species_dir, paste0("date=", date_str, ".parquet"))
    message(paste("Writing", output_file))
    write_parquet(df, output_file)
  }
}

message("Sample data created successfully in structured parquet format!")
message("This data can be used for testing the RTBM app's new data loading approach.")

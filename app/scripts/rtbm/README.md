# Real-Time Bird Monitoring (RTBM) Data Update Script (`update_rtbm_parquet_data.R`)

## Overview

This R script automates the process of fetching, processing, and storing bird occurrence data for the Real-Time Bird Monitoring (RTBM) component of the BioDT Shiny application. It retrieves daily species occurrence data (originally in TIFF format) from an S3-compatible source, converts it into an efficient Parquet format suitable for analysis and visualization within the application, and stores it locally.

The script is designed to be run periodically (e.g., daily via a cron job) to keep the local data store up-to-date.

## Features

-   **Efficient Data Fetching:** Retrieves the list of available data files (keys) from the S3 bucket once per run, optimizing performance for date range processing.
-   **Local Caching:** Downloads TIFF files to a local cache directory (`app/data/rtbm/cache/`) to avoid redundant downloads.
-   **Data Conversion:** Converts geospatial TIFF files into tabular Parquet format (`app/data/rtbm/parquet/`).
-   **Coordinate System Handling:** Automatically detects and converts coordinates from the source projection (likely meters) to WGS84 (latitude/longitude) during the conversion process.
-   **Data Validation:** Checks processed Parquet files for valid data (e.g., positive intensity values) and removes empty or invalid files.
-   **Parallel Processing:** Utilizes multiple CPU cores (if available) to speed up the computationally intensive raster-to-point conversion process.
-   **Flexible Date Processing:** Supports processing data for a single specific date, a date range, or defaults to processing from a defined start date up to the previous day.
-   **Configuration:** Uses command-line arguments for specifying dates and forcing data refresh.
-   **Dependency Management:** Lists required R packages.
-   **Bird Metadata:** Optionally downloads and uses `bird_info.json` for mapping species names if the local file is missing.

## Dependencies

The script requires the following R packages:

-   `httr2` (for HTTP requests)
-   `xml2` (for parsing S3 bucket listings)
-   `stringr` (for string manipulation)
-   `fs` (for file system operations)
-   `purrr` (for functional programming utilities)
-   `terra` (for geospatial data handling - raster operations)
-   `arrow` (for reading/writing Parquet files)
-   `lubridate` (for date/time manipulation)
-   `jsonlite` (for reading `bird_info.json`)
-   `optparse` (for command-line argument parsing)
-   `dplyr` (for data manipulation)
-   `sf` (for Simple Features, used in coordinate transformations)
-   `stats` (provides `complete.cases`)
-   `parallel` (for parallel processing)

Ensure these packages are installed in the R environment where the script will run.

## Configuration

-   **`PROJECT_ROOT`:** The script assumes it is run from the project's root directory (`biodt-shiny`). Paths are constructed relative to this root.
-   **`RTBM_CACHE_PATH`:** Path to the local cache for downloaded TIFF files (Default: `app/data/rtbm/cache/`).
-   **`RTBM_PARQUET_PATH`:** Path to store the output Parquet files (Default: `app/data/rtbm/parquet/`).
-   **`BIRD_INFO_JSON_PATH`:** Path to the local `bird_info.json` file (Default: `app/data/rtbm/bird_info.json`).
-   **`BIRD_INFO_URL`:** URL to download `bird_info.json` if it's missing locally.
-   **`RTBM_TIFF_BUCKET_URL_BASE`:** **Important:** This needs to be set to the correct base URL of the S3 bucket containing the daily TIFF files (e.g., `https://your-bucket-url/daily/`). The script currently uses a placeholder.
-   **`DEFAULT_START_DATE`:** The default date to start processing from if no start date is specified via command line (Default: `"2025-01-16"`).

## Usage

Run the script from the project root directory using `Rscript`.

```bash
# Example: Process data for a specific date, forcing redownload/overwrite
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -d YYYY-MM-DD -f

# Example: Process data for a specific date range
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -s YYYY-MM-DD -e YYYY-MM-DD

# Example: Process data from the default start date up to yesterday
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R

# Example: Process data from a specific start date up to yesterday
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -s YYYY-MM-DD
```

### Command Line Options

-   `-d`, `--date YYYY-MM-DD`: Process data only for this specific date. Overrides `-s` and `-e`.
-   `-s`, `--start-date YYYY-MM-DD`: Start date for processing range. Defaults to `DEFAULT_START_DATE` if omitted.
-   `-e`, `--end-date YYYY-MM-DD`: End date for processing range. Defaults to yesterday if omitted.
-   `-f`, `--force`: Force redownload of TIFF files (ignore cache) and overwrite existing Parquet files. Default is `FALSE` (use cache, don't overwrite).
-   `-h`, `--help`: Show help message and exit.

## Workflow

For each date in the target sequence:

1.  **Fetch S3 Keys (Once per run):** Retrieves the full list of file keys from the S3 bucket.
2.  **Filter Keys:** Identifies keys relevant to the current processing date.
3.  **Download & Cache TIFFs:** Downloads TIFF files corresponding to the filtered keys if they are not already present in the cache (or if `--force` is used).
4.  **Convert TIFF to Parquet:**
    -   Iterates through cached TIFF files for the date.
    -   Reads the raster data using `terra`.
    -   Extracts the scientific name from the filename.
    -   Converts raster data to a data frame of points (lon, lat, intensity, scientific_name, date).
    -   **Transforms Coordinates:** Converts coordinates to WGS84 (EPSG:4326).
    -   **Validates Data:** Checks if the resulting data frame has valid points with positive intensity.
    -   Writes the valid data to a partitioned Parquet file (`app/data/rtbm/parquet/date=YYYY-MM-DD/`). If `--force` is used, existing files are overwritten. Invalid or empty files are removed.
5.  **Logging:** Provides informative messages about progress, downloads, conversions, errors, and final summaries.

## Output

-   Cached TIFF files in `app/data/rtbm/cache/<YYYY-MM-DD>/`.
-   Partitioned Parquet dataset in `app/data/rtbm/parquet/`. The data is partitioned by `date` (e.g., `app/data/rtbm/parquet/date=2025-04-15/part-0.parquet`). Each Parquet file contains columns: `lon`, `lat`, `intensity`, `scientific_name`.

## Notes

-   The script relies heavily on the `terra` package for efficient raster processing and `arrow` for Parquet handling.
-   Parallel processing significantly speeds up the conversion step. The number of cores used is typically `total cores - 1`.
-   Ensure the S3 bucket URL (`RTBM_TIFF_BUCKET_URL_BASE`) is correctly configured.

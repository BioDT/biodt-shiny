# RTBM Parquet Data Update Script

This R script (`update_rtbm_parquet_data.R`) automates the process of fetching, converting, and validating Real-time Bird Monitoring (RTBM) occurrence data for use in the BioDT Shiny application.

## Purpose

*   **Downloads** daily species occurrence TIFF files from an S3-compatible object storage.
*   **Converts** the downloaded geospatial TIFF rasters into a tabular Parquet format, partitioned by species and date.
*   **Transforms Coordinates:** Converts coordinates from their original projection (likely meters) to WGS84 (latitude/longitude).
*   **Caches** downloaded TIFF files locally (`app/data/rtbm/cache/`) to minimize redundant downloads.
*   **Validates** the final Parquet files (`app/data/rtbm/parquet/`), removing files that are empty or contain no valid occurrences (e.g., zero positive intensity).
*   **Checks S3 Source:** In validation mode (`-v`), ensures a corresponding source TIFF exists on S3 before attempting to reprocess missing or invalid local Parquet files.

## Usage

Run the script from the main project root directory:

```bash
# Process data for yesterday (default behavior)
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R

# Process data for a specific single date
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -d YYYY-MM-DD

# Process data for a specific date range
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -s YYYY-MM-DD -e YYYY-MM-DD

# Force reprocessing for a specific single date (ignores cache and existing Parquet)
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -f -d YYYY-MM-DD

# Validate existing Parquet files for a date range, reprocessing only if necessary and source exists
Rscript app/scripts/rtbm/update_rtbm_parquet_data.R -v -s YYYY-MM-DD -e YYYY-MM-DD
```

**Arguments:**

*   `-d`, `--date`: Specific date to process (YYYY-MM-DD). Cannot be used with `--start-date` or `--end-date`.
*   `-s`, `--start-date`: Start date for processing range (YYYY-MM-DD). Must be used *together* with `--end-date`.
*   `-e`, `--end-date`: End date for processing range (YYYY-MM-DD). Must be used *together* with `--start-date`.
*   `-f`, `--force`: Force reprocessing, ignoring cache and existing files.
*   `-v`, `--validate`: Run in validation mode. Checks local files against S3 and reprocesses only missing/invalid ones *if* the source TIFF exists on S3.

## Configuration

*   The S3 endpoint base URL is configured within the script (`S3_ENDPOINT_BASE`).
*   The list of required bird species is read from `app/data/rtbm/bird_info.json`.

## Dependencies

Requires the following R packages: `httr2`, `stringr`, `fs`, `purrr`, `terra`, `arrow`, `lubridate`, `jsonlite`, `optparse`. Ensure they are installed in the R environment where the script is executed.

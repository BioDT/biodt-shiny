box::use(
  leaflet[evalFormula, labelFormat, invokeMethod, getMapData],
  grDevices[col2rgb],
  stars[st_warp, read_stars],
  sf[st_crs],
  terra[rast, mask, project, ifel],
  httr[HEAD],
  rvest[read_html, html_nodes, html_text],
  xml2[read_html],
  dplyr[`%>%`],
  readr[read_delim],
)

#' @export
addLegend_decreasing <- function(map, position = c("topright", "bottomright", "bottomleft", "topleft"),
                                 pal, values, na.label = "NA", bins = 7, colors,
                                 opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                 title = NULL, className = "info legend", layerId = NULL,
                                 group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) {
      stop("You must provide either 'pal' or 'colors' (not both)")
    }
    if (missing(title) && inherits(values, "formula")) {
      title <- deparse(values[[2]])
    }
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) {
      warning("'bins' is ignored because the palette type is not numeric")
    }
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) pretty(values, bins) else bins
      if (length(bins) > 2) {
        if (!all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))) {
          stop("The vector of breaks 'bins' must be equally spaced")
        }
      }
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1]) / (r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE) {
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      } else {
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    } else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n]) / 2
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      } else {
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    } else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n]) / 2, na.rm = TRUE)
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      } else {
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    } else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE) {
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      } else {
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    } else {
      stop("Palette function not supported")
    }
    if (!any(is.na(values))) {
      na.color <- NULL
    }
  } else {
    if (length(colors) != length(labels)) {
      stop("'colors' and 'labels' must be of the same length")
    }
  }
  legend <- list(
    colors = I(unname(colors)), labels = I(unname(labels)),
    na_color = na.color, na_label = na.label, opacity = opacity,
    position = position, type = type, title = title, extra = extra,
    layerId = layerId, className = className, group = group
  )
  invokeMethod(map, data, "addLegend", legend)
}

# Function to load and process the GeoTIFF file from OPeNDAP
#' @export
process_raster_file <- function(file_path) {
  cat("Trying to read file:", file_path, "\n")
  tryCatch(
    {
      r <- terra::rast(file_path)
      cat("Class of raster object:", class(r), "\n")
      print(r)
      r_proj <- terra::project(r, "EPSG:3857")
      return(r_proj)
    },
    error = function(e) {
      warning(paste("Failed to read raster file:", file_path, "\n", e$message))
      return(NULL)
    }
  )
}

# Helper function to check if a URL exists using httr
#' @export
url.exists <- function(url) {
  res <- tryCatch(httr::HEAD(url, timeout(5)), error = function(e) NULL)
  if (is.null(res)) {
    return(FALSE)
  }
  res$status_code == 200
}

# habitat vector for mapping files
#' @export
habitat_mapping <- c(
  "Forests" = "hab1",
  "Open forests" = "hab2",
  "Scrub" = "hab3",
  "Natural grasslands" = "hab4a",
  "Human maintained grasslands" = "hab4b",
  "Wetland" = "hab10",
  "Ruderal habitats" = "hab12a",
  "Agricultural habitats" = "hab12b"
)

# getting the available versions of the pDT from OpenDAP
#' @export
get_available_versions <- function() {
  page <- read_html("http://opendap.biodt.eu/ias-pdt/")
  folders <- page |>
    html_nodes("a") |>
    html_text()
  version_folders <- folders[grepl("^\\d", folders)]
  version_folders <- gsub("/$", "", version_folders)
  sort(version_folders, decreasing = TRUE)
}

# check the validity of the pDT version
#' @export
check_valid_version <- function(version) {
  habitat <- habitat_mapping[1]
  test_url <- paste0("http://opendap.biodt.eu/ias-pdt/", version, "/outputs/", habitat, "/predictions/Prediction_Summary_Shiny.RData")
  tryCatch(
    {
      tmp <- new.env()
      load(url(test_url), envir = tmp)
      !is.null(tmp$Prediction_Summary_Shiny)
    },
    error = function(e) FALSE
  )
}

# base url for data loading
#' @export
get_base_url <- function(version) {
  paste0("http://opendap.biodt.eu/ias-pdt/", version, "/")
}

# function to get the distribution species data
#' @export
get_species_file_from_pa <- function(habitat_code, ias_id, obs_type, base_url) {
  url <- paste0(
    base_url, "outputs/", habitat_code,
    "/observed_distribution/PA.txt"
  )

  tryCatch(
    {
      df <- readr::read_delim(url(url),
        delim = "\t",
        show_col_types = FALSE, trim_ws = TRUE
      )

      names(df) <- tolower(trimws(names(df))) # normalise headers
      needed <- c("ias_id", "pa_file", "pa_model_file")
      if (!all(needed %in% names(df))) {
        return(NULL)
      }

      row <- dplyr::filter(df, ias_id == !!ias_id)
      if (nrow(row) == 0) {
        return(NULL)
      }

      tif_file <- if (obs_type == "full") row$pa_file[1] else row$pa_model_file[1]
      tif_file <- trimws(tif_file)

      if (is.na(tif_file) || tif_file == "") NULL else tif_file
    },
    error = function(e) {
      warning("Failed to read PA.txt: ", e$message)
      NULL
    }
  )
}

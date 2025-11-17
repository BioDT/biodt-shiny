# Code here is based on the https://github.com/BioDT/uc-ces-recreation

box::use(
  leaflet[
    leaflet,
    addProviderTiles,
    setView,
    addLegend,
    hideGroup,
    showGroup,
    addRasterImage,
    addLayersControl,
    layersControlOptions,
    clearGroup,
  ],
  leaflet.extras[
    addFullscreenControl,
    addDrawToolbar,
    drawRectangleOptions,
    drawShapeOptions,
    editToolbarOptions,
  ],
  htmlwidgets[onRender],
  terra[project],
)

get_base_layers <- function() {
  list(
    "Street" = "Esri.WorldStreetMap",
    "Topographical" = "Esri.WorldTopoMap",
    "Satellite" = "Esri.WorldImagery",
    "Greyscale" = "Esri.WorldGrayCanvas"
  )
}

addBaseLayers <- function(map, base_layers) {
  for (layer in base_layers) {
    map <- addProviderTiles(map, layer, group = layer) |>
      hideGroup(layer)
  }
  return(map)
}

setup_map <- function(pal = NULL, button_container_id = "button_container", ns = NULL) {
  base_layers <- get_base_layers()

  # Create the base map
  map <- leaflet() |>
    setView(lng = -4.2026, lat = 56.4907, zoom = 7) |>
    addBaseLayers(base_layers) |>
    showGroup(base_layers[[1]])

  # Only add legend if pal is provided
  if (!is.null(pal)) {
    map <- map |>
      addLegend(
        title = "Values",
        position = "bottomright",
        values = c(0, 1),
        pal = pal
      )
  }

  map <- map |>
    addFullscreenControl() |>
    addDrawToolbar(
      targetGroup = "drawnItems",
      singleFeature = TRUE,
      editOptions = editToolbarOptions(
        edit = FALSE,
        remove = FALSE,
        selectedPathOptions = FALSE
      ),
      rectangleOptions = drawRectangleOptions(
        shapeOptions = drawShapeOptions(
          color = "black",
          weight = 2,
          fillOpacity = 0
        ),
        metric = TRUE,
        showArea = TRUE
      ),
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE
    ) |>
    onRender(
      paste0(
        "
      function(el, x) {
        var map = this;
        var grayscale = false;

        // Override Leaflet.draw's area calculation to use km² instead of hectares
        if (L.GeometryUtil && L.GeometryUtil.geodesicArea) {
          L.GeometryUtil.readableArea = function(area, isMetric) {
            var areaStr;
            
            if (isMetric) {
              if (area >= 1000000) {
                areaStr = (area / 1000000).toFixed(2) + ' km²';
              } else {
                areaStr = area.toFixed(2) + ' m²';
              }
            } else {
              area /= 0.836127; // Square yards in 1 meter
              if (area >= 3097600) { // 3097600 square yards in 1 square mile
                areaStr = (area / 3097600).toFixed(2) + ' mi²';
              } else {
                areaStr = area.toFixed(2) + ' yd²';
              }
            }
            
            return areaStr;
          };
        }

        function applyGrayscale(layer) {
          if (layer instanceof L.TileLayer) {
            // Check if this layer is in the biodiversity group
            var isBiodiversityLayer = false;
            map.eachLayer(function(l) {
              if (l === layer && l.options && l.options.attribution === 'GBIF') {
                isBiodiversityLayer = true;
              }
            });
            
            // Only apply grayscale if it's not the biodiversity layer
            if (!isBiodiversityLayer) {
              layer.getContainer().style.filter = grayscale ? 'grayscale(100%)' : 'none';
            }
          }
        }

        function toggleGrayscale() {
          grayscale = !grayscale;
          map.eachLayer(applyGrayscale);
          btn = document.getElementById('grayscale-toggle-",
        button_container_id,
        "')
          var icon = btn.querySelector('i');
            if (grayscale) {
                  icon.className = 'fa-solid fa-droplet';
                  console.log('click happened');
                } else {
                  icon.className = 'fa-solid fa-droplet-slash';
                   console.log('no click');
                }
        }


        map.on('layeradd', function(e) {
          applyGrayscale(e.layer);
        });

        L.Control.GrayScaleControl = L.Control.extend({
          onAdd: function(map) {
            var btn = L.DomUtil.create('button', 'btn btn-default action-button toggle-button shiny-bound-input');
            btn.title = 'Toggle Grayscale';
            btn.id = 'grayscale-toggle-",
        button_container_id,
        "';

            var icon = document.createElement('i');
            icon.className = 'fa-solid fa-droplet-slash';

            btn.appendChild(icon);

            btn.onclick = toggleGrayscale;


            var container = document.getElementById('",
        button_container_id,
        "');
            if (container) {
              container.appendChild(btn);
            }
            
            return L.DomUtil.create('div');
          }
        });

        new L.Control.GrayScaleControl().addTo(map);

        // Custom recreation compute button
        L.Control.ComputeRecreationControl = L.Control.extend({
          onAdd: function(map) {
            var btn = L.DomUtil.create('button', 'btn btn-default action-button shiny-bound-input');
            btn.title = 'Compute Recreation';
            btn.id = '",
        if (!is.null(ns)) paste0(ns("compute_recreation")) else "compute_recreation",
        "';

            var icon = document.createElement('i');
            icon.className = 'fa-solid fa-rotate-right';

            btn.appendChild(icon);

            btn.onclick = function() {
              // Trigger Shiny input
              if (typeof Shiny !== 'undefined') {
                Shiny.setInputValue('",
        if (!is.null(ns)) paste0(ns("compute_recreation")) else "compute_recreation",
        "', Math.random());
              }
            };

            var container = document.getElementById('",
        button_container_id,
        "');
            if (container) {
              container.appendChild(btn);
            }
            
            return L.DomUtil.create('div');
          }
        });

        new L.Control.ComputeRecreationControl().addTo(map);

      }"
      )
    )

  return(map)
}


##' Add raster overlay layers and a native layers control
##'
##' @param map A `leaflet` map object or a `leafletProxy`.
##' @param layers A named list of raster-like objects (length 5 expected).
##' @param opacity Numeric opacity for raster layers (0-1).
##' @param clear_existing If TRUE, attempt to clear any existing overlay groups with the same names.
##' @return A leaflet map (or proxy) with the new overlay layers and a layers control.
update_map <- function(proxy, layers, opacity = 0.8, clear_existing = TRUE, pal = "Spectral") {
  # Expect a leafletProxy object
  if (is.null(layers) || length(layers) == 0) {
    return(proxy)
  }

  if (!inherits(proxy, "leaflet_proxy")) {
    stop("`proxy` must be a leafletProxy object created with leafletProxy().")
  }

  if (is.null(names(layers)) || any(names(layers) == "")) {
    names(layers) <- paste0("Layer ", seq_along(layers))
  }

  # Rename technical layer names to human-readable names
  if ("SLSRA" %in% names(layers)) {
    names(layers)[names(layers) == "SLSRA"] <- "Landscape & Land Cover"
  }
  if ("FIPS_N" %in% names(layers)) {
    names(layers)[names(layers) == "FIPS_N"] <- "Natural Features"
  }
  if ("FIPS_I" %in% names(layers)) {
    names(layers)[names(layers) == "FIPS_I"] <- "Infrastructure"
  }

  # Clear existing groups if requested (best-effort)
  if (clear_existing) {
    for (g in names(layers)) {
      tryCatch(
        {
          proxy <- clearGroup(proxy, g)
        },
        error = function(e) NULL
      )
    }
  }

  # Add each raster as an overlay group using the proxy
  for (nm in names(layers)) {
    lyr <- layers[[nm]]

    proxy <- tryCatch(
      {
        # Only pass colors parameter if pal is not NULL
        if (!is.null(pal)) {
          addRasterImage(
            proxy,
            lyr,
            opacity = opacity,
            group = nm,
            colors = pal
          )
        } else {
          addRasterImage(
            proxy,
            lyr,
            opacity = opacity,
            group = nm
          )
        }
      },
      error = function(e) {
        proxy
      }
    )
  }

  return(proxy)
}

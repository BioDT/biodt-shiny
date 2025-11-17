box::use(
  shiny,
  biodt.recreation[load_config],
  stats[setNames],
)

make_sliders <- function(component, ns = function(x) x) {
  config <- load_config()

  layer_info <- setNames(config[["Description"]], config[["Name"]])
  layer_names <- names(layer_info)
  group_names <- list(
    SLSRA_LCM = "Land Cover",
    SLSRA_Designations = "Official Designations",
    FIPS_N_Landform = "Land Formations",
    FIPS_N_Slope = "Slopes",
    FIPS_N_Soil = "Soil Type",
    FIPS_I_RoadsTracks = "Roads and Tracks",
    FIPS_I_NationalCycleNetwork = "National Cycle Network",
    FIPS_I_LocalPathNetwork = "Local Path Network",
    Water_Lakes = "Lakes",
    Water_Rivers = "Rivers"
  )

  layer_names_this_component <- layer_names[startsWith(layer_names, component)]
  groups_this_component <- group_names[startsWith(names(group_names), component)]

  sliders <- lapply(layer_names_this_component, function(layer_name) {
    shiny$sliderInput(
      inputId = ns(layer_name),
      label = layer_info[[layer_name]],
      min = 0,
      max = 10,
      value = 0,
      round = TRUE,
      ticks = FALSE
    )
  })

  lapply(names(groups_this_component), function(group) {
    # NOTE: very hacky method to group all designations, which actually stem
    # from different layers. It may actually be preferable to hard-code the groups
    # into a new column of config.csv
    if (group == "SLSRA_Designations") {
      sliders_this_group <- sliders[!startsWith(layer_names_this_component, "SLSRA_LCM")]
    } else {
      sliders_this_group <- sliders[startsWith(layer_names_this_component, group)]
    }

    n <- length(sliders_this_group)
    sliders_left <- sliders_this_group[seq(1, n, by = 2)]
    sliders_right <- if (n > 1) {
      sliders_this_group[seq(2, n, by = 2)]
    } else {
      list()
    }

    shiny$div(
      style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-top: 5px; margin-bottom: 5px;",
      shiny$h4(group_names[group]),
      shiny$fluidRow(
        shiny$column(width = 6, sliders_left),
        shiny$column(width = 6, sliders_right)
      )
    )
  })
}

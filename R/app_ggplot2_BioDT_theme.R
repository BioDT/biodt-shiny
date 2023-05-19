# ggplot2 theme for BioDT ####
#libraries
library(tidyverse)
library(unikn) # to check the color palette
library(extrafont)  # Load the extrafont package for font management

# Import the Roboto font
font_import(pattern = "Roboto")
loadfonts()

# Define custom color palette
## I stick with 8 colors as it is usually recommended to not go above this number of colors in one plot
biodt_palette <- c( "#ebdec5", "#e4cb96", "#dbbc76", "#bc6c25",
                   "#a2c5ac", "#82927d", "#596a42", "#414f2f")

# Visualizing the palette
unikn::seecol(pal = biodt_palette)

# Create a custom BioDT theme #####
theme_biodt <- function(){
  theme(
    # text settings
    text = element_text(family = "Roboto", size = 14),
    # panel settings
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_blank(),
    # axis settings
    axis.ticks = element_blank(),
    # axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    # legend settings
    legend.box.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = "#fff9ef"),
    # plot settings
    plot.background = element_rect(fill = "#fff9ef"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Roboto"),
    plot.caption = element_text(size = 9, hjust = 1, family = "Roboto", face = "italic"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    )
}

# Custom scale_fill_ and scale_color_ function ####
biodt_color <- function(...) {
  biodt_colors <- c(
    `c_brown1` = "#ebdec5", 
    `c_brown2` = "#e4cb96", 
    `c_brown3` = "#dbbc76", 
    `c_brown4` = "#bc6c25",
    `c_green1` = "#a2c5ac",
    `c_green2` = "#82927d",
    `c_green3` = "#596a42",
    `c_green4` = "#414f2f"
    )
  cols <- c(...)
  
  if (is.null(cols))
    return (biodt_colors)
  biodt_colors[cols]
  }

biodt_palette <- function(palette = "main", ...) {
  biodt_palettes <- list(
    `main` = biodt_color("c_brown4", "c_brown3", "c_brown2", "c_brown1", "c_green1", "c_green2", "c_green3", "c_green4"),
    `highlight` = biodt_color("c_brown1", "c_green4"),
    `greens` = biodt_color("c_green1", "c_green2", "c_green3", "c_green4"),
    `browns` = biodt_color("c_brown1", "c_brown2", "c_brown3", "c_brown4"),
    `light` = biodt_color("c_brown1", "c_brown2", "c_green1", "c_green2"),
    `dark` = biodt_color("c_brown3", "c_brown4", "c_green3", "c_green4")
    )
  
  biodt_palettes[[palette]]
  }

palette_gen <- function(palette = "main", direction = 1) {
  function(n) {
    if (n > length(biodt_palette(palette)))
    warning("Use a palette with the appropriated number of levels.")
    else {
      all_colors <- biodt_palette(palette)
      all_colors <- unname(unlist(all_colors))
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      color_list <- all_colors[1:n]
    }
  }
  }

palette_gen_c <- function(palette = "main", direction = 1, ...) {
  pal <- biodt_palette(palette)
  pal <- if (direction >= 0) pal else rev(pal)
  colorRampPalette(pal, ...)
  }

scale_fill_biodt <- function(palette = "main", direction = 1, ...) {
  ggplot2::discrete_scale(
  "fill", "biodt",
  palette_gen(palette, direction),
  ...
  )
  }

scale_colour_biodt <- function(palette = "main", direction = 1, ...) {
  ggplot2::discrete_scale(
  "colour", "biodt",
  palette_gen(palette, direction),
  ...
  )
  }

scale_color_biodt <- scale_colour_biodt

scale_color_biodt_c <- function(palette = "main", direction = 1, ...) {
  pal <- palette_gen_c(palette = palette, direction = direction)
  scale_color_gradientn(colors = pal(256), ...)
  }

# Testing the the theme and the scale_fill_biodt and scale_color_biodt functions ----
## Dummy dataset
dummy_df <- tibble(species = c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8"),
                   abundance = c(10, 15, 13, 4, 8, 9, 12, 9))

dummy_df %>% 
  ggplot(aes(x = species, y = abundance, fill = species, color = species)) +
  geom_col() +
  coord_flip() +
  labs(title = "BioDT theme demonstration",
       subtitle = "It contains custom color palette",
       caption = "by: Souza AT") +
  scale_fill_biodt(palette = "main") +
  scale_color_biodt(palette = "main") +
  theme_biodt()

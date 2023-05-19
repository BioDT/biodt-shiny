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
unikn::seecol(pal = biodt_palette, n = 3) # modify the n to see how the palette will be with less than 8 colors

# Create a custom BioDT theme #####
theme_biodt <- function(){
  theme(
    # text settings
    text = element_text(family = "Roboto", size = 14),
    # panel settings
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    # axis settings
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black"),
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

# Testing the theme
iris |>
  group_by(Species) |> 
  summarise(ave_sepal_length = mean(Sepal.Length, na.rm = TRUE)) |> 
  ggplot(aes(x = Species, y = ave_sepal_length, fill = Species)) +
  geom_col() +
  coord_flip() +
  labs(title = "BioDT theme demonstration",
       subtitle = "It contains custom color palette",
       caption = "by: Souza AT") +
  scale_fill_manual(values = biodt_palette) +
  scale_color_manual(values = biodt_palette) +
  theme_biodt()

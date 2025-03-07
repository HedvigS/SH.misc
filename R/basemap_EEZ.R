#' basmap layer for ggplot with EEZ, Pacific-centered.
#' @param xlim numerical vector of lenght 2
#' @param ylim numerical vector of lenght 2
#' @param south description
#' @param colour_land defaults to "white",
#' @param colour_ocean defaults to "gray",
#' @param colour_ocean_EEZ defaults to "lightgray",
#' @param colour_border_land defaults to "darkgray",
#' @param colour_border_eez defaults to "#949494",
#' @param colour_lakes defaults to "lightgray"
#' @import dplyr
#' @import usethis
#' @import ggplot2
#' @import magrittr
#' @note EEZ data from SOVEREIGN1 layer of Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632. File name: World_EEZ_v11_20191118_LR/eez_v11_lowres.shp
#' @author Siva Kalyan & Hedvig Skirgård
#' @export

basemap_EEZ <- function(xlim = c(-30, 330),
                                ylim = c(-60, 90),
                                south = "up",
                                padding  = 0.1,
                                colour_land = "white",
                                colour_ocean = "gray",
                                colour_ocean_EEZ = "lightgray",
                                colour_border_land = "darkgray",
                                colour_border_eez = "#949494",
                                colour_lakes = "lightgray"
){

    if(!south %in% c("up", "down")){
        stop("Argument south has to be either 'up' or 'down'.")

    }

#load("sysdata.rda")
#library(tidyverse)


  #loading data that is necessary for both kinds of plots
  world <- ggplot2::map_data("world", wrap = c(-30,330))
  lakes <- ggplot2::map_data("lakes", wrap = c(-30,330))

#  colour_border_eez = "lightgray"
#  colour_border_land = "white"

    p <- ggplot() +
        geom_polygon(data = EEZ_shp_df,
                     mapping = aes(x = long, y = lat, group = group),
                     color = colour_border_eez,
                     fill = colour_ocean_EEZ,
                     linewidth = 0.2 + 1 * padding) +
      geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group),
                   col = colour_border_land, fill = colour_land,
                   linewidth = 0.25) +
      geom_polygon(data = lakes, mapping = aes(x = long, y = lat, group = group),
                   col = colour_border_land, fill = colour_lakes,
                   linewidth = 0.25 + 1 * padding) +
      geom_polygon(data = lakes, mapping = aes(x = long, y = lat, group = group),
                   col = colour_lakes, fill = colour_lakes,
                   linewidth = 0 + padding) +  theme_minimal() +
      theme(legend.position="none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title=element_blank(),
            axis.line = element_blank(),
            panel.background = element_rect(colour = NA, fill = colour_ocean),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin=grid::unit(c(0,0,-1,-1), "mm")
      )


  if(south == "up"){
    p <- p  +
      coord_equal(xlim = rev(xlim), ylim = rev(ylim)) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))

  }
    if(south == "down"){
    p <- p  +
      coord_equal(xlim = xlim, ylim = ylim) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))

  }


p
}
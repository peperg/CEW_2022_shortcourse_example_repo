###################################################################
#################     BOREAL Colour Palettes  ######################
###################################################################

## Aproach modified from https://boreal.svbtle.com/creating-corporate-colour-palettes-for-ggplot2


boreal_cols <- c(`control_blue` = "#277da1", 
                 `grey_blue`    = "#577590", 
                 `green_blue`   = "#43aa8b", 
                 `lime_green`   = "#90be6d", 
                 `yellow`       = "#f9c74f", 
                 `light_orange` = "#f8961e", 
                 `dark_orange`  = "#f3722c", 
                 `red`          = "#f94144")


#' Function to extract boreal colours as hex codes
#'
#' @param ... Character names of boreal_colours 
#'

boreal_colours <- function(...){
 
  cols <- c(...)
  
  if (is.null(cols))
    return (boreal_cols)
  
  boreal_cols[cols]

}


boreal_pals <- list(
  `boreal_full` = boreal_colours("control_blue", "grey_blue", "green_blue", "lime_green", "yellow", "light_orange", "dark_orange", "red"),
  `boreal_2_controls` = boreal_colours("control_blue", "control_blue", "grey_blue", "green_blue", "lime_green", "yellow", "light_orange", "dark_orange", "red"),
  `boreal_intensive` = boreal_colours("control_blue", "grey_blue", "yellow", "red"))


#' Return function to interpolate a boreal colour palette
#'
#' @param palette Character name of palette in boreal_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colourRampPalette()
#'

boreal_palettes <- function(palette = "boreal_full", reverse = FALSE, ...) {
    
  pal <- boreal_pals[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
  

#' colour scale constructor for boreal colours
#'
#' @param palette Character name of palette in boreal_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_colour_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_boreal <- function(palette = "boreal_full", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- boreal_palettes(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("boreal_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for boreal colours
#'
#' @param palette Character name of palette in boreal_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_boreal <- function(palette = "boreal_full", discrete = TRUE, reverse = FALSE, ...) {
  pal <- boreal_palettes(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("boreal_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
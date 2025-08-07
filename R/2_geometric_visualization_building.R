#' Visualize Unstructured and Structured Building Spatial Data Side by Side
#'
#' This function takes two spatial datasets of buildings (unstructured and structured)
#' and creates a side-by-side comparison plot with a black divider in between.
#' It uses `ggplot2` and `patchwork` for visualization.
#'
#' @param buildings_unstructured An sf object representing unstructured buildings.
#' @param buildings_structured An sf object representing structured buildings.
#' @return A patchwork ggplot object combining the two spatial plots.
#' @import ggplot2
#' @importFrom patchwork plot_layout
#' @examples
#' \dontrun{
#' # Assuming you have spatial sf objects loaded:
#' plot_building_comparison(buildings_unstructured, buildings_structured)
#' }
#' @export
plot_building_comparison <- function(buildings_unstructured, buildings_structured) {
  # Plot for unstructured buildings
  p1 <- ggplot() +
    geom_sf(data = buildings_unstructured) +
    ggtitle("Unstructured") +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot")

  # Plot for structured buildings
  p2 <- ggplot() +
    geom_sf(data = buildings_structured) +
    ggtitle("Structured") +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot")

  # Black divider between plots
  divider <- ggplot() +
    theme_void() +
    theme(panel.background = element_rect(fill = "black"))

  # Combine plots side by side with a narrow black divider in between
  combined_plot <- (p1 + divider + p2) + plot_layout(ncol = 3, widths = c(1, 0.03, 1))

  return(combined_plot)
}

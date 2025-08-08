#' Plot Building Area Bins for Structured and Unstructured Buildings
#'
#' This function reads two spatial datasets of buildings (structured and unstructured),
#' classifies the building footprints based on their area into predefined bins,
#' and visualizes them side-by-side as maps with a shared bar chart legend.
#'
#' The datasets are assumed to be in a projected Coordinate Reference System (CRS),
#' so that `st_area()` produces accurate area measurements in square meters.
#'
#' @param unstructured_path Character. File path to the unstructured buildings GeoPackage.
#' @param structured_path Character. File path to the structured buildings GeoPackage.
#'
#' @return A combined ggplot object displaying the two maps side-by-side with a shared legend bar below.
#'
#' @import sf
#' @import dplyr
#' @import units
#' @import ggplot2
#' @import patchwork
#' @import viridis
#'
#' @examples
#' \dontrun{
#' plot_building_area_bins(
#'   unstructured_path = "path/to/NewCairo_unstructured.gpkg",
#'   structured_path = "path/to/NewCairo_structured.gpkg"
#' )
#' }
#'
#' @export
plot_building_area_bins <- function(unstructured_path, structured_path) {
  # Load datasets
  buildings_unstructured <- sf::st_read(unstructured_path)
  buildings_structured <- sf::st_read(structured_path)

  # Helper function to classify buildings into area bins
  create_bins <- function(data) {
    geom_col <- attr(data, "sf_column")
    data %>%
      dplyr::mutate(
        area = as.numeric(sf::st_area(.data[[geom_col]])),
        area_bin = cut(
          area,
          breaks = c(0, 16, 48, 90, 145, 255, 400, Inf),
          labels = c("<16 m²", "16-48 m²", "48-90 m²", "90-145 m²",
                     "145-255 m²", "255-400 m²", "400+ m²"),
          right = FALSE
        )
      )
  }

  buildings_unstructured <- create_bins(buildings_unstructured)
  buildings_structured <- create_bins(buildings_structured)

  # Define consistent bin colors
  bin_levels <- levels(buildings_unstructured$area_bin)
  bin_colors <- viridis::viridis(length(bin_levels), direction = -1)

  simple_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Plots without legends
  p_unstructured_bin <- ggplot2::ggplot(buildings_unstructured) +
    ggplot2::geom_sf(ggplot2::aes(fill = area_bin), color = NA) +
    ggplot2::scale_fill_manual(values = bin_colors, drop = FALSE) +
    ggplot2::labs(title = "Unstructured Buildings") +
    simple_theme

  p_structured_bin <- ggplot2::ggplot(buildings_structured) +
    ggplot2::geom_sf(ggplot2::aes(fill = area_bin), color = NA) +
    ggplot2::scale_fill_manual(values = bin_colors, drop = FALSE) +
    ggplot2::labs(title = "Structured Buildings") +
    simple_theme

  # Prepare legend data
  midpoints <- c(8, 32, 69, 117.5, 200, 327.5, 500)
  legend_df <- data.frame(
    bin = factor(bin_levels, levels = bin_levels),
    mid_area = midpoints,
    color = bin_colors
  )

  legend_bar <- ggplot2::ggplot(legend_df, ggplot2::aes(x = mid_area, y = bin, fill = bin)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = bin_colors) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(x = "Building Area (m²)", y = NULL, title = "Building Area Bins") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 11, margin = ggplot2::margin(t = 10)),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
    )

  # Combine maps and legend with patchwork
  final_plot <- (p_unstructured_bin | p_structured_bin) / legend_bar +
    patchwork::plot_layout(heights = c(10, 2))

  print(final_plot)
}

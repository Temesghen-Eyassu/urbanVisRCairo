# Avoid NSE (non-standard evaluation) warnings for CRAN
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("bin", "center", "count"))
}

#' Calculate Area, Perimeter, and Compactness for Spatial Polygons
#'
#' @param data An \code{sf} object containing polygon geometries.
#' @return An \code{sf} object with additional columns: \code{area}, \code{perimeter}, \code{compactness}.
#' @importFrom sf st_geometry st_area st_length st_cast
#' @importFrom dplyr mutate
#' @export
calculate_attributes <- function(data) {
  geom <- sf::st_geometry(data)
  area <- as.numeric(sf::st_area(geom))
  perimeter <- as.numeric(sf::st_length(sf::st_cast(geom, "MULTILINESTRING")))
  compactness <- 4 * pi * area / (perimeter^2)

  dplyr::mutate(data,
                area = area,
                perimeter = perimeter,
                compactness = compactness)
}

#' Create a Combined Map and Histogram Plot for a Spatial Variable
#'
#' @param data An \code{sf} object with spatial features.
#' @param variable Variable name as string.
#' @param title_prefix String prefix for plot titles.
#' @param fill_option Viridis palette option.
#' @param bins Number of histogram bins.
#' @return A \code{patchwork} object combining map and histogram.
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c geom_col labs theme_void theme_minimal theme element_text ggtitle aes
#' @importFrom dplyr mutate group_by summarise filter n
#' @importFrom magrittr %>%
#' @import patchwork
#' @export
plot_map_and_histogram <- function(data, variable, title_prefix, fill_option = "C", bins = 30) {
  # map
  map_plot <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), color = NA) +
    ggplot2::scale_fill_viridis_c(option = fill_option,
                                  name = ifelse(variable == "area", "Area (m²)", "Compactness")) +
    ggplot2::ggtitle(paste0(title_prefix, ": ", tools::toTitleCase(variable))) +
    ggplot2::theme_void()

  # histogram data
  hist_data <- data %>%
    dplyr::mutate(bin = cut(.data[[variable]], breaks = bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count = dplyr::n(),
      center = mean(as.numeric(sub("\\((.+),.*", "\\1", as.character(bin))) +
                      as.numeric(sub(".*,(.+)]", "\\1", as.character(bin)))) / 2,
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(center))

  # histogram plot
  hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = center, y = count, fill = center)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c(option = fill_option, name = NULL) +
    ggplot2::labs(x = tools::toTitleCase(variable), y = "Count", title = "Histogram") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  map_plot + hist_plot
}

#' Analyze and Plot Structured vs. Unstructured Buildings
#'
#' Reads the two building datasets, calculates shape metrics, and produces a 4-panel plot.
#'
#' @param unstructured_path Path to unstructured buildings GeoPackage.
#' @param structured_path Path to structured buildings GeoPackage.
#' @return A \code{patchwork} object combining all plots.
#' @importFrom sf st_read
#' @export
#' @examples
#' \dontrun{
#' final_plot <- analyze_building(
#'   unstructured_path = "path/to/NewCairo_unstructured.gpkg",
#'   structured_path = "path/to/NewCairo_structured.gpkg"
#' )
#' print(final_plot)
#' }
analyze_building <- function(unstructured_path, structured_path) {
  buildings_structured <- sf::st_read(structured_path, quiet = TRUE)
  buildings_unstructured <- sf::st_read(unstructured_path, quiet = TRUE)

  buildings_structured <- calculate_attributes(buildings_structured)
  buildings_unstructured <- calculate_attributes(buildings_unstructured)

  structured_compactness_plot <- plot_map_and_histogram(buildings_structured, "compactness", "Structured")
  structured_area_plot <- plot_map_and_histogram(buildings_structured, "area", "Structured", fill_option = "D")
  unstructured_compactness_plot <- plot_map_and_histogram(buildings_unstructured, "compactness", "Unstructured")
  unstructured_area_plot <- plot_map_and_histogram(buildings_unstructured, "area", "Unstructured", fill_option = "D")

  structured_compactness_plot / structured_area_plot / unstructured_compactness_plot / unstructured_area_plot
}

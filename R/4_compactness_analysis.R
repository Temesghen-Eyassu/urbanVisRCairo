#' Plot Compactness Analysis for Structured and Unstructured Buildings
#'
#' This function reads two spatial datasets (structured and unstructured buildings),
#' calculates a compactness metric (based on area and perimeter),
#' filters values to [0,1], summarizes statistics, and produces scientific-grade
#' comparative visualizations using `ggplot2` and `patchwork`.
#'
#' @param unstructured_path File path to the unstructured buildings GeoPackage (.gpkg).
#' @param structured_path File path to the structured buildings GeoPackage (.gpkg).
#' @return A patchwork plot object containing maps and a histogram/density plot.
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @importFrom patchwork plot_layout plot_annotation
#' @examples
#' \dontrun{
#' plot_compactness_area_graphs(
#'   unstructured_path = "path/to/unstructured.gpkg",
#'   structured_path = "path/to/structured.gpkg"
#' )
#' }
#' @export
plot_compactness_area_graphs <- function(unstructured_path, structured_path) {
  # Step 1: Read spatial datasets
  buildings_unstructured <- sf::st_read(unstructured_path, quiet = TRUE)
  buildings_structured <- sf::st_read(structured_path, quiet = TRUE)

  # Step 2: Compute Compactness (bounded between 0 and 1)
  buildings_structured <- buildings_structured %>%
    dplyr::mutate(compactness = 4 * pi * as.numeric(st_area(.)) / as.numeric(st_perimeter(.))^2) %>%
    dplyr::filter(compactness <= 1)

  buildings_unstructured <- buildings_unstructured %>%
    dplyr::mutate(compactness = 4 * pi * as.numeric(st_area(.)) / as.numeric(st_perimeter(.))^2) %>%
    dplyr::filter(compactness <= 1)

  # Step 3: Compactness Maps
  map_structured <- ggplot(buildings_structured) +
    geom_sf(aes(fill = compactness), color = NA, alpha = 0.8) +
    scale_fill_viridis_c(option = "C", limits = c(0, 1), name = "Compactness") +
    ggtitle("Structured Buildings") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  map_unstructured <- ggplot(buildings_unstructured) +
    geom_sf(aes(fill = compactness), color = NA, alpha = 0.8) +
    scale_fill_viridis_c(option = "C", limits = c(0, 1), name = "Compactness") +
    ggtitle("Unstructured Buildings") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  # Step 4: Histogram and Density Plot
  hist_combined <- ggplot(dplyr::bind_rows(
    dplyr::mutate(buildings_structured, type = "Structured"),
    dplyr::mutate(buildings_unstructured, type = "Unstructured")
  ), aes(x = compactness, fill = type)) +
    geom_histogram(aes(y = after_stat(density)), bins = 15, alpha = 0.6,
                   position = "identity", color = "white") +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("Structured" = "steelblue", "Unstructured" = "darkred")) +
    labs(x = "Compactness", y = "Density", title = "Distribution Comparison") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Step 5: Combine all plots
  final_plot <- (map_structured + map_unstructured) / hist_combined +
    patchwork::plot_layout(heights = c(1, 1.5)) +
    patchwork::plot_annotation(
      title = "Compactness Analysis of Structured vs. Unstructured Buildings",
      subtitle = "Higher compactness (closer to 1) indicates more circular/regular shapes",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    )

  return(final_plot)
}

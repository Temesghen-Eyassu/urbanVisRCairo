#' Hexbin Area Analysis of Structured vs. Unstructured Buildings
#'
#' This function performs a spatial hexbin aggregation and visualization
#' comparing building areas between structured and unstructured building datasets.
#' It reads two GeoPackage files, calculates building areas, creates a shared
#' hexagonal grid over their combined bounding box, filters to a core study area,
#' aggregates building counts and average area per hex, filters out outliers,
#' and produces comparative hexbin maps and density plots.
#'
#' @param unstructured_path Character. File path to the unstructured buildings GeoPackage (.gpkg).
#' @param structured_path Character. File path to the structured buildings GeoPackage (.gpkg).
#' @param hex_size Numeric. Cell size for hexagonal grid in map units (default 150).
#' @param buffer_dist Numeric. Distance for inward buffer to define core study area (default 150).
#'
#' @return A combined patchwork ggplot object showing:
#' \itemize{
#'   \item Hexbin map of average building area for unstructured buildings (core area)
#'   \item Density plot of average building area for unstructured buildings
#'   \item Hexbin map of average building area for structured buildings (core area)
#'   \item Density plot of average building area for structured buildings
#' }
#'
#' @details
#' The function creates a hexagonal grid over the union of the bounding boxes of
#' the two datasets, filters to core hexes inside an inward buffer of the convex hull,
#' then spatially joins buildings to hexes. It computes counts and mean area per hex,
#' filters low-count and outlier hexes, and plots maps and density plots with a shared
#' color scale.
#'
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @import viridis
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' plot_hexbin_area_analysis(
#'   unstructured_path = "path/to/unstructured.gpkg",
#'   structured_path = "path/to/structured.gpkg"
#' )
#' }
plot_hexbin_area_analysis <- function(unstructured_path, structured_path, hex_size = 150, buffer_dist = 150) {
  # No library() calls here!

  # Step 1: Read spatial data
  buildings_unstructured <- sf::st_read(unstructured_path, quiet = TRUE)
  buildings_structured <- sf::st_read(structured_path, quiet = TRUE)

  # Step 2: Calculate area for each building
  buildings_unstructured$area <- as.numeric(sf::st_area(buildings_unstructured))
  buildings_structured$area <- as.numeric(sf::st_area(buildings_structured))

  # Step 3: Create shared bounding box and hexagonal grid
  bbox_union <- sf::st_union(
    sf::st_as_sfc(sf::st_bbox(buildings_unstructured)),
    sf::st_as_sfc(sf::st_bbox(buildings_structured))
  )

  hex_grid <- sf::st_make_grid(
    bbox_union,
    cellsize = hex_size,
    square = FALSE,
    what = "polygons"
  ) %>% sf::st_sf()

  hex_grid$hex_id <- seq_len(nrow(hex_grid))  # Unique hex ID

  # Step 4: Create core area using inward buffer
  all_buildings <- rbind(buildings_unstructured, buildings_structured)
  study_area_hull <- all_buildings %>% sf::st_union() %>% sf::st_convex_hull()
  core_area <- sf::st_buffer(study_area_hull, dist = -buffer_dist)

  # Step 5: Filter hex grid to core hexes
  hex_grid_core <- hex_grid[sf::st_within(hex_grid, core_area, sparse = FALSE), ]

  # Step 6: Spatial join and aggregate - Unstructured
  hex_join_unstructured <- sf::st_join(hex_grid_core, buildings_unstructured, join = sf::st_intersects, left = FALSE)

  hex_stats_unstructured <- hex_join_unstructured %>%
    dplyr::group_by(hex_id) %>%
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      count = dplyr::n(),
      avg_area = mean(area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    sf::st_as_sf()

  # Step 7: Spatial join and aggregate - Structured
  hex_join_structured <- sf::st_join(hex_grid_core, buildings_structured, join = sf::st_intersects, left = FALSE)

  hex_stats_structured <- hex_join_structured %>%
    dplyr::group_by(hex_id) %>%
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      count = dplyr::n(),
      avg_area = mean(area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    sf::st_as_sf()

  # Step 8: Filter out low-count and outlier hexes
  hex_stats_unstructured_filtered <- hex_stats_unstructured %>% dplyr::filter(count >= 1)
  hex_stats_structured_filtered <- hex_stats_structured %>%
    dplyr::filter(count >= 1, avg_area < stats::quantile(avg_area, 0.99, na.rm = TRUE))

  # Step 9: Shared color scale
  shared_scale <- range(
    c(hex_stats_unstructured_filtered$avg_area, hex_stats_structured_filtered$avg_area),
    na.rm = TRUE
  )

  # Step 10: Hexbin maps with very light blue for unstructured and very light yellow for structured

  map_unstructured <- ggplot2::ggplot(hex_stats_unstructured_filtered) +
    ggplot2::geom_sf(ggplot2::aes(fill = avg_area), color = NA) +
    ggplot2::scale_fill_gradient(
      low = "#deebf7", high = "#3182bd",  # very light blue to moderate blue
      name = "Avg Area", limits = shared_scale
    ) +
    ggplot2::labs(title = "Hexbin Map – Unstructured (Core Area)") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.1, face = "bold"))

  map_structured <- ggplot2::ggplot(hex_stats_structured_filtered) +
    ggplot2::geom_sf(ggplot2::aes(fill = avg_area), color = NA) +
    ggplot2::scale_fill_gradient(
      low = "#fff7bc", high = "#fec44f",  # very light yellow to darker yellow/orange
      name = "Avg Area", limits = shared_scale
    ) +
    ggplot2::labs(title = "Hexbin Map – Structured (Core Area)") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.1, face = "bold"))

  # Step 11: Density plots with matching colors (very light blue and very light yellow)

  g_density_unstructured <- ggplot2::ggplot(hex_stats_unstructured_filtered, ggplot2::aes(x = avg_area)) +
    ggplot2::geom_density(fill = "#7faedc", alpha = 0.3) +  # lighter blue, more transparent
    ggplot2::labs(title = "Unstructured – Avg Area Density", x = "Avg Area (m²)", y = "Density") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  g_density_structured <- ggplot2::ggplot(hex_stats_structured_filtered, ggplot2::aes(x = avg_area)) +
    ggplot2::geom_density(fill = "#fde8a0", alpha = 0.3) +  # lighter yellow, more transparent
    ggplot2::labs(title = "Structured – Avg Area Density", x = "Avg Area (m²)", y = "Density") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))



  # Step 12: Combine plots
  final_plot <- (map_unstructured | g_density_unstructured) /
    (map_structured | g_density_structured) +
    patchwork::plot_layout(ncol = 1)

  print(final_plot)
}

library(patchwork)

plot_hexbin_area_analysis(
  unstructured_path = "E:/EAGLE/Second_Semester/Scientific_Graphs/Five_ classes/NewCairo_unstructured.gpkg",
  structured_path = "E:/EAGLE/Second_Semester/Scientific_Graphs/Five_ classes/NewCairo_structured.gpkg"
)


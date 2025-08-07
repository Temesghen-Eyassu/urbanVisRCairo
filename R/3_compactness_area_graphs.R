if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("bin", "center", "count"))
}

#' Calculate Area, Perimeter, and Compactness for Spatial Polygons
#'
#' Given an \code{sf} object with polygon geometries, this function calculates
#' three spatial attributes:
#' \itemize{
#'   \item \code{area}: Polygon area (square meters).
#'   \item \code{perimeter}: Polygon perimeter length (meters).
#'   \item \code{compactness}: Compactness index \eqn{4 \pi * \mathrm{area} / \mathrm{perimeter}^2}.
#' }
#' The function returns the original \code{sf} object augmented with these new columns.
#'
#' @param data An \code{sf} object containing polygon geometries.
#'
#' @return An \code{sf} object with additional columns: \code{area}, \code{perimeter}, and \code{compactness}.
#'
#' @importFrom sf st_geometry st_area st_length st_cast
#' @importFrom dplyr mutate
#' @export
calculate_attributes <- function(data) {
  geom <- sf::st_geometry(data)
  area <- as.numeric(sf::st_area(geom))
  perimeter <- as.numeric(sf::st_length(sf::st_cast(geom, "MULTILINESTRING")))
  compactness <- 4 * pi * area / (perimeter^2)

  data %>%
    dplyr::mutate(
      area = area,
      perimeter = perimeter,
      compactness = compactness
    )
}

#' Create a Combined Map and Histogram Plot for a Spatial Variable
#'
#' This function produces a combined plot showing:
#' \itemize{
#'   \item A spatial map with polygons colored by a numeric variable.
#'   \item A histogram showing the distribution of the variable.
#' }
#' The map and histogram share the same Viridis color scale for consistent coloring.
#'
#' @param data An \code{sf} object with spatial features.
#' @param variable Character string specifying the numeric variable column name.
#' @param title_prefix String prefix for plot titles (e.g., "Structured", "Unstructured").
#' @param fill_option Viridis color scale option (default: "C").
#' @param bins Number of bins for histogram (default: 30).
#'
#' @return A combined \code{patchwork} object of the map and histogram plots.
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c geom_col labs theme_void theme_minimal theme element_text ggtitle aes
#' @importFrom dplyr mutate group_by summarise filter n
#' @import patchwork
#' @export
plot_map_and_histogram <- function(data, variable, title_prefix, fill_option = "C", bins = 30) {
  var_data <- data[[variable]]

  # Map plot
  map_plot <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), color = NA) +
    ggplot2::scale_fill_viridis_c(option = fill_option,
                                  name = paste0(ifelse(variable == "area", "Area (m²)", "Compactness"))) +
    ggplot2::ggtitle(paste0(title_prefix, ": ", tools::toTitleCase(variable))) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.1))

  # Histogram data processing
  hist_data <- data %>%
    dplyr::mutate(bin = cut(.data[[variable]], breaks = bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count = dplyr::n(),
      center = mean(
        as.numeric(sub("\\((.+),.*", "\\1", as.character(bin))) +
          as.numeric(sub(".*,(.+)]", "\\1", as.character(bin)))
      ) / 2
    ) %>%
    dplyr::filter(!is.na(center))

  # Histogram plot
  hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = center, y = count, fill = center)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c(option = fill_option, name = NULL) +
    ggplot2::labs(x = tools::toTitleCase(variable), y = "Count", title = "Histogram") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.1)
    )

  # Return combined patchwork plot
  return(map_plot + hist_plot)
}

#' Example Workflow: Read Spatial Data, Compute Attributes, and Plot
#'
#' This example demonstrates how to read structured and unstructured building polygons
#' from GeoPackage files, compute spatial attributes, and visualize them.
#'
#' @details
#' This code is intended as example usage, not a function.
#' You will need to adjust file paths to your environment.
#'
#' @importFrom sf st_read
#' @import patchwork
#' @examples
#' \dontrun{
#' buildings_structured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_structured.gpkg")
#' buildings_unstructured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_unstructured.gpkg")
#'
#' buildings_structured <- calculate_attributes(buildings_structured)
#' buildings_unstructured <- calculate_attributes(buildings_unstructured)
#'
#' structured_compactness_plot <- plot_map_and_histogram(buildings_structured, "compactness", "Structured")
#' structured_area_plot <- plot_map_and_histogram(buildings_structured, "area", "Structured", fill_option = "D")
#' unstructured_compactness_plot <- plot_map_and_histogram(buildings_unstructured, "compactness", "Unstructured")
#' unstructured_area_plot <- plot_map_and_histogram(buildings_unstructured, "area", "Unstructured", fill_option = "D")
#'
#' final_plot <- (structured_compactness_plot / structured_area_plot /
#'                unstructured_compactness_plot / unstructured_area_plot)
#' print(final_plot)
#' }

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("bin", "center", "count"))
}

#' Calculate Area, Perimeter, and Compactness for Spatial Polygons
#'
#' Given an \code{sf} object with polygon geometries, this function calculates
#' three spatial attributes:
#' \itemize{
#'   \item \code{area}: Polygon area (square meters).
#'   \item \code{perimeter}: Polygon perimeter length (meters).
#'   \item \code{compactness}: Compactness index \eqn{4 \pi * \mathrm{area} / \mathrm{perimeter}^2}.
#' }
#' The function returns the original \code{sf} object augmented with these new columns.
#'
#' @param data An \code{sf} object containing polygon geometries.
#'
#' @return An \code{sf} object with additional columns: \code{area}, \code{perimeter}, and \code{compactness}.
#'
#' @importFrom sf st_geometry st_area st_length st_cast
#' @importFrom dplyr mutate
#' @export
calculate_attributes <- function(data) {
  geom <- sf::st_geometry(data)
  area <- as.numeric(sf::st_area(geom))
  perimeter <- as.numeric(sf::st_length(sf::st_cast(geom, "MULTILINESTRING")))
  compactness <- 4 * pi * area / (perimeter^2)

  data %>%
    dplyr::mutate(
      area = area,
      perimeter = perimeter,
      compactness = compactness
    )
}

#' Create a Combined Map and Histogram Plot for a Spatial Variable
#'
#' This function produces a combined plot showing:
#' \itemize{
#'   \item A spatial map with polygons colored by a numeric variable.
#'   \item A histogram showing the distribution of the variable.
#' }
#' The map and histogram share the same Viridis color scale for consistent coloring.
#'
#' @param data An \code{sf} object with spatial features.
#' @param variable Character string specifying the numeric variable column name.
#' @param title_prefix String prefix for plot titles (e.g., "Structured", "Unstructured").
#' @param fill_option Viridis color scale option (default: "C").
#' @param bins Number of bins for histogram (default: 30).
#'
#' @return A combined \code{patchwork} object of the map and histogram plots.
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c geom_col labs theme_void theme_minimal theme element_text ggtitle aes
#' @importFrom dplyr mutate group_by summarise filter n
#' @import patchwork
#' @export
plot_map_and_histogram <- function(data, variable, title_prefix, fill_option = "C", bins = 30) {
  var_data <- data[[variable]]

  # Map plot
  map_plot <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), color = NA) +
    ggplot2::scale_fill_viridis_c(option = fill_option,
                                  name = paste0(ifelse(variable == "area", "Area (m²)", "Compactness"))) +
    ggplot2::ggtitle(paste0(title_prefix, ": ", tools::toTitleCase(variable))) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.1))

  # Histogram data processing
  hist_data <- data %>%
    dplyr::mutate(bin = cut(.data[[variable]], breaks = bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count = dplyr::n(),
      center = mean(
        as.numeric(sub("\\((.+),.*", "\\1", as.character(bin))) +
          as.numeric(sub(".*,(.+)]", "\\1", as.character(bin)))
      ) / 2
    ) %>%
    dplyr::filter(!is.na(center))

  # Histogram plot
  hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = center, y = count, fill = center)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c(option = fill_option, name = NULL) +
    ggplot2::labs(x = tools::toTitleCase(variable), y = "Count", title = "Histogram") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.1)
    )

  # Return combined patchwork plot
  return(map_plot + hist_plot)
}

#' Example Workflow: Read Spatial Data, Compute Attributes, and Plot
#'
#' This example demonstrates how to read structured and unstructured building polygons
#' from GeoPackage files, compute spatial attributes, and visualize them.
#'
#' @details
#' This code is intended as example usage, not a function.
#' You will need to adjust file paths to your environment.
#'
#' @importFrom sf st_read
#' @import patchwork
#' @examples
#' \dontrun{
#' buildings_structured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_structured.gpkg")
#' buildings_unstructured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_unstructured.gpkg")
#'
#' buildings_structured <- calculate_attributes(buildings_structured)
#' buildings_unstructured <- calculate_attributes(buildings_unstructured)
#'
#' structured_compactness_plot <- plot_map_and_histogram(buildings_structured, "compactness", "Structured")
#' structured_area_plot <- plot_map_and_histogram(buildings_structured, "area", "Structured", fill_option = "D")
#' unstructured_compactness_plot <- plot_map_and_histogram(buildings_unstructured, "compactness", "Unstructured")
#' unstructured_area_plot <- plot_map_and_histogram(buildings_unstructured, "area", "Unstructured", fill_option = "D")
#'
#' final_plot <- (structured_compactness_plot / structured_area_plot /
#'                unstructured_compactness_plot / unstructured_area_plot)
#' print(final_plot)
#' }


if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("bin", "center", "count"))
}

#' Calculate Area, Perimeter, and Compactness for Spatial Polygons
#'
#' Given an \code{sf} object with polygon geometries, this function calculates
#' three spatial attributes:
#' \itemize{
#'   \item \code{area}: Polygon area (square meters).
#'   \item \code{perimeter}: Polygon perimeter length (meters).
#'   \item \code{compactness}: Compactness index \eqn{4 \pi * \mathrm{area} / \mathrm{perimeter}^2}.
#' }
#' The function returns the original \code{sf} object augmented with these new columns.
#'
#' @param data An \code{sf} object containing polygon geometries.
#'
#' @return An \code{sf} object with additional columns: \code{area}, \code{perimeter}, and \code{compactness}.
#'
#' @importFrom sf st_geometry st_area st_length st_cast
#' @importFrom dplyr mutate
#' @export
calculate_attributes <- function(data) {
  geom <- sf::st_geometry(data)
  area <- as.numeric(sf::st_area(geom))
  perimeter <- as.numeric(sf::st_length(sf::st_cast(geom, "MULTILINESTRING")))
  compactness <- 4 * pi * area / (perimeter^2)

  data %>%
    dplyr::mutate(
      area = area,
      perimeter = perimeter,
      compactness = compactness
    )
}

#' Create a Combined Map and Histogram Plot for a Spatial Variable
#'
#' This function produces a combined plot showing:
#' \itemize{
#'   \item A spatial map with polygons colored by a numeric variable.
#'   \item A histogram showing the distribution of the variable.
#' }
#' The map and histogram share the same Viridis color scale for consistent coloring.
#'
#' @param data An \code{sf} object with spatial features.
#' @param variable Character string specifying the numeric variable column name.
#' @param title_prefix String prefix for plot titles (e.g., "Structured", "Unstructured").
#' @param fill_option Viridis color scale option (default: "C").
#' @param bins Number of bins for histogram (default: 30).
#'
#' @return A combined \code{patchwork} object of the map and histogram plots.
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c geom_col labs theme_void theme_minimal theme element_text ggtitle aes
#' @importFrom dplyr mutate group_by summarise filter n
#' @import patchwork
#' @export
plot_map_and_histogram <- function(data, variable, title_prefix, fill_option = "C", bins = 30) {
  var_data <- data[[variable]]

  # Map plot
  map_plot <- ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), color = NA) +
    ggplot2::scale_fill_viridis_c(option = fill_option,
                                  name = paste0(ifelse(variable == "area", "Area (m²)", "Compactness"))) +
    ggplot2::ggtitle(paste0(title_prefix, ": ", tools::toTitleCase(variable))) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.1))

  # Histogram data processing
  hist_data <- data %>%
    dplyr::mutate(bin = cut(.data[[variable]], breaks = bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count = dplyr::n(),
      center = mean(
        as.numeric(sub("\\((.+),.*", "\\1", as.character(bin))) +
          as.numeric(sub(".*,(.+)]", "\\1", as.character(bin)))
      ) / 2
    ) %>%
    dplyr::filter(!is.na(center))

  # Histogram plot
  hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = center, y = count, fill = center)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c(option = fill_option, name = NULL) +
    ggplot2::labs(x = tools::toTitleCase(variable), y = "Count", title = "Histogram") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.1)
    )

  # Return combined patchwork plot
  return(map_plot + hist_plot)
}

#' Example Workflow: Read Spatial Data, Compute Attributes, and Plot
#'
#' This example demonstrates how to read structured and unstructured building polygons
#' from GeoPackage files, compute spatial attributes, and visualize them.
#'
#' @details
#' This code is intended as example usage, not a function.
#' You will need to adjust file paths to your environment.
#'
#' @importFrom sf st_read
#' @import patchwork
#' @examples
#' \dontrun{
#' buildings_structured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_structured.gpkg")
#' buildings_unstructured <- sf::st_read("E:/EAGLE/Second_Semester/Scientific_Graphs/Five_classes/NewCairo_unstructured.gpkg")
#'
#' buildings_structured <- calculate_attributes(buildings_structured)
#' buildings_unstructured <- calculate_attributes(buildings_unstructured)
#'
#' structured_compactness_plot <- plot_map_and_histogram(buildings_structured, "compactness", "Structured")
#' structured_area_plot <- plot_map_and_histogram(buildings_structured, "area", "Structured", fill_option = "D")
#' unstructured_compactness_plot <- plot_map_and_histogram(buildings_unstructured, "compactness", "Unstructured")
#' unstructured_area_plot <- plot_map_and_histogram(buildings_unstructured, "area", "Unstructured", fill_option = "D")
#'
#' final_plot <- (structured_compactness_plot / structured_area_plot /
#'                unstructured_compactness_plot / unstructured_area_plot)
#' print(final_plot)
#' }




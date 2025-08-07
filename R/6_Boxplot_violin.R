#' Compare Building Compactness: Structured vs. Unstructured Areas
#'
#' This function reads two building datasets (structured and unstructured),
#' calculates building compactness, and creates a combined visualization with
#' boxplots, density plots, and violin plots comparing compactness distributions.
#'
#' @param unstructured_path Character. File path to unstructured buildings GeoPackage (.gpkg).
#' @param structured_path Character. File path to structured buildings GeoPackage (.gpkg).
#'
#' @return A patchwork ggplot object combining boxplot, density plot, and violin plot.
#'
#' @importFrom sf st_read st_area st_perimeter
#' @importFrom dplyr mutate bind_rows group_by summarise
#' @import ggplot2
#' @import viridis
#' @import patchwork
#' @import ggpubr
#' @export
#'
#' @examples
#' \dontrun{
#'   p <- compare_building_compactness(
#'     "path/to/NewCairo_unstructured.gpkg",
#'     "path/to/NewCairo_structured.gpkg"
#'   )
#'   print(p)
#'   ggsave("compactness_comparison.png", p, width = 12, height = 10, dpi = 300)
#' }
compare_building_compactness <- function(unstructured_path, structured_path) {
  # Load data
  buildings_unstructured <- sf::st_read(unstructured_path, quiet = TRUE)
  buildings_structured <- sf::st_read(structured_path, quiet = TRUE)

  # Calculate compactness
  buildings_unstructured <- buildings_unstructured %>%
    dplyr::mutate(
      area = sf::st_area(.),
      perimeter = sf::st_perimeter(.),
      compactness = 4 * pi * as.numeric(area) / (as.numeric(perimeter)^2),
      type = "Unstructured"
    )

  buildings_structured <- buildings_structured %>%
    dplyr::mutate(
      area = sf::st_area(.),
      perimeter = sf::st_perimeter(.),
      compactness = 4 * pi * as.numeric(area) / (as.numeric(perimeter)^2),
      type = "Structured"
    )

  # Combine data
  combined_data <- dplyr::bind_rows(buildings_unstructured, buildings_structured)

  # Summary stats
  summary_stats <- combined_data %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(
      mean = mean(compactness, na.rm = TRUE),
      sd = sd(compactness, na.rm = TRUE),
      .groups = 'drop'
    )

  base_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 20, unit = "pt")
    )

  # Boxplot
  p1 <- ggplot2::ggplot(combined_data, ggplot2::aes(x = type, y = compactness, fill = type)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      label = "p.format",
      label.x = 1.5,
      label.y = max(combined_data$compactness, na.rm = TRUE) * 1.05
    ) +
    ggplot2::geom_text(
      data = summary_stats,
      ggplot2::aes(x = type, y = mean, label = sprintf("μ = %.2f ± %.2f", mean, sd)),
      vjust = -3,
      size = 3.5,
      color = "black"
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,
      size = 4,
      color = "red"
    ) +
    ggplot2::scale_fill_viridis_d(option = "C", direction = -1) +
    ggplot2::labs(
      title = "Boxplot of Compactness",
      x = "Building Type",
      y = "Compactness (4πA/P²)"
    ) +
    base_theme +
    ggplot2::ylim(0, max(combined_data$compactness, na.rm = TRUE) * 1.1)

  # Density plot
  p2 <- ggplot2::ggplot(combined_data, ggplot2::aes(x = compactness, fill = type)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::geom_vline(
      data = summary_stats,
      ggplot2::aes(xintercept = mean, color = type),
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::scale_color_viridis_d(option = "C") +
    ggplot2::labs(
      title = "Density of Compactness",
      x = "Compactness (4πA/P²)",
      y = "Density",
      fill = "Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  # Violin plot
  p3 <- ggplot2::ggplot(combined_data, ggplot2::aes(x = type, y = compactness, fill = type)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      label = "p.format",
      label.x = 1.5,
      label.y = max(combined_data$compactness, na.rm = TRUE) * 1.05
    ) +
    ggplot2::geom_text(
      data = summary_stats,
      ggplot2::aes(x = type, y = mean, label = sprintf("μ = %.2f", mean)),
      vjust = -3,
      size = 3.5,
      color = "black"
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,
      size = 4,
      color = "red"
    ) +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::labs(
      title = "Violin Plot of Compactness",
      x = "Building Type",
      y = "Compactness (4πA/P²)"
    ) +
    base_theme +
    ggplot2::ylim(0, max(combined_data$compactness, na.rm = TRUE) * 1.1)

  # Combine plots
  final_plot <- (p1 | p2) / p3 +
    patchwork::plot_annotation(
      title = "Comparison of Building Compactness: Structured vs. Unstructured Areas",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14))
    )

  return(final_plot)
}






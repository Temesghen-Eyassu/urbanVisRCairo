#' Read spatial datasets
#'
#' @param unstructured_path Path to the unstructured buildings GeoPackage file.
#' @param structured_path Path to the structured buildings GeoPackage file.
#' @return A list with two sf objects: unstructured and structured buildings.
#' @import sf
#' @export
read_buildings <- function(unstructured_path, structured_path) {
  buildings_unstructured <- sf::st_read(unstructured_path, quiet = TRUE)
  buildings_structured <- sf::st_read(structured_path, quiet = TRUE)
  list(unstructured = buildings_unstructured, structured = buildings_structured)
}

#' Check spatial consistency of building datasets
#'
#' @param buildings A list with two sf objects as returned by read_buildings.
#' @return A list containing CRS and bounding boxes of each dataset.
#' @import sf
#' @importFrom utils str
#' @export
check_spatial_consistency <- function(buildings) {
  list(
    crs_unstructured = sf::st_crs(buildings$unstructured),
    crs_structured = sf::st_crs(buildings$structured),
    bbox_unstructured = sf::st_bbox(buildings$unstructured),
    bbox_structured = sf::st_bbox(buildings$structured),
    structure_unstructured = str(buildings$unstructured),
    structure_structured = str(buildings$structured)
  )
}


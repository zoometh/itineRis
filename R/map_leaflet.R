#' Create an HTML interactive map for the site distribution
#' @name map_leaflet
#' @description
#'
#' @param df.path the path to the dataframe. This dataframe must have at least three columns:
#' one for the site name (by defaut 'site'), two for the coordinates (by default, 'x' and 'y')
#' @param export.plot if TRUE, export the map
#' @param out.plot the name of the output map
#' @param dirOut path of the output directory
#'
#'
#' @return a leaflet interactive map
#'
#' @examples
#'
#' map_leaflet(color.point = "blue")
#'
#' @export
map_leaflet <- function(df.path = paste0(system.file(package = "itineRis"), "/extdata/sites_coords.tsv"),
                        site_column = "site",
                        x_column = "x",
                        y_column = "y",
                        color.point = "red",
                        radius.point = 5,
                        export.plot = F,
                        out.plot = "map_sites.html",
                        dirOut = paste0(system.file(package = "itineRis"), "/results/")){
  if(DescTools::SplitPath(df.path)$extension == "tsv"){sep = "\t"}
  if(DescTools::SplitPath(df.path)$extension == "csv"){sep = ";"}
  df <- read.table(df.path, sep = sep, header = T)
  print(message(paste0("column names of '",
                       DescTools::SplitPath(df.path)$fullfilename,
                       "' are: ", paste0(colnames(df), collapse = ", "))))
  #lbl <- '<a href = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/EAMENA-0164997.geojson">EAMENA-0164997.geojson</a>'
  ea.map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$"Esri.WorldImagery", group = "Ortho") %>%
    leaflet::addProviderTiles(leaflet::providers$"OpenStreetMap", group = "OSM") %>%
    leaflet::addCircleMarkers(
      lng = df[, x_column],
      lat = df[, y_column],
      weight = 1,
      radius = radius.point,
      popup = df[, site_column],
      color = color.point,
      fillOpacity = 1,
      opacity = 1) %>%
    leaflet::addLayersControl(
      baseGroups = c("Ortho", "OSM"),
      position = "topright") %>%
    leaflet::addScaleBar(position = "bottomright")
  if (export.plot) {
    dir.create(dirOut, showWarnings = FALSE)
    htmlwidgets::saveWidget(ea.map, out.plot)
    print(paste("interactive map '", out.plot, "' created in '", dirOut, "'"))
  } else {
    print(ea.map)
  }
}


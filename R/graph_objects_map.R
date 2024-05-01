#' Create a Create graph and community detection of objects typo
#'
#' @name graph_objects_map
#'
#' @description Read XLSX dataset of objects hosted on Google Drive
#'
#' @param ggdocument.url The filename, ex: "DATA_01_IT_VC_2.xlsx"
#' @param authentificate Authentificate when TRUE
#'
#' @return A dataframe of objects
#'
#' @examples
#'
#'
#' @export
graph_objects_map <- function(data.per = NA,
                              a.graph = 1,
                              ext.title = TRUE,
                              bbox = c(left = 7, bottom = 44, right = 11.2, top = 46),
                              zoom = 8,
                              stadiamaps.token = "aa5c9739-90c7-410b-9e9b-6c904df6e4dd",
                              stadiamaps.maptype = "stamen_terrain_background",
                              # a.graph = 1,
                              # ext.title = TRUE,
                              # LD_OB_TY_ST = TRUE,
                              # only_Fibule = FALSE,
                              # only_Others = FALSE, # selection on objects
                              verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  ggmap::register_stadiamaps(stadiamaps.token, write = FALSE)
  bck.stamen.sp <- ggmap::get_stadiamap(bbox = bbox,
                                        zoom = zoom,
                                        maptype = stadiamaps.maptype)
  # raw map
  if(verbose){
    print(paste0("Creates raw map"))
  }
  site.names <- as.data.frame(unique(data.per[ , c("Lieu-dit")]))
  site.names <- site.names[ , 1]
  nds.sites <- data.frame(name = site.names,
                          shape = rep("circle", length(site.names)),
                          color = rep("grey", length(site.names)),
                          font = rep(2, length(site.names))
  )
  sites.coordinates <- as.data.frame(unique(data.per[data.per[["Lieu-dit"]] %in% nds.sites$name,
                                                     c("Lieu-dit", "x", "y")]))
  names(sites.coordinates) <- c("name", "x", "y")
  # sites.coordinates$x <- as.numeric(sites.coordinates$x)
  # sites.coordinates$y <- as.numeric(sites.coordinates$y)
  # sites.xy.community <- sites.coordinates
  sites.coordinates.sf <- sf::st_as_sf(sites.coordinates, coords = c("x", "y"))
  sf::st_crs(sites.coordinates.sf) <- 4236
  # names(sites.coordinates.sf) <- c("name")
  sp.local <- bck.stamen.sp %>%
    ggmap::ggmap() +
    # ggplot2::ggtitle(per.title) +
    # ggplot2::geom_point(data=sites.xy.community,
    #                     ggplot2::aes(x = x, y = y), color = "black",
    #                     size = 2) +
    ggplot2::geom_sf(data = sites.coordinates.sf, inherit.aes = FALSE) +
    ggrepel::geom_text_repel(data=sites.coordinates,
                             ggplot2::aes(x = x, y = y, label = name),
                             color = "black",
                             cex = 3.5,
                             max.overlaps = Inf) +
    # ggplot2::geom_sf_text(data = sites.coordinates.sf,
    #                       aes(label = name),
    #                       size = 3,
    #                       color = "black",
    #                       inherit.aes = FALSE) +
    ggspatial::annotation_north_arrow(location = "tl",
                                      which_north = "true",
                                      height = unit(1, "cm"),
                                      width = unit(1, "cm"),
                                      pad_x = unit(0.1, "in"),
                                      pad_y = unit(0.1, "in"),
                                      style = ggspatial::north_arrow_fancy_orienteering) +
    ggspatial::annotation_scale(location = "br",
                                height = unit(0.2, "cm"),
                                width_hint = 0.2) +
    ggplot2::labs(title = a.graph,
                  caption = paste0("Périodes (av. n. è.): ", ext.title),
                  x = "Longitude",
                  y = "Latitude")
  return(sp.local)
}

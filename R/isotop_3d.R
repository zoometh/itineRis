#' Create an HTML interactive chart with the values of 3 different isotops
#' @name isotop_3d
#' @description get a dataframe and create a 3D plot
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param vars the names of the columns where the isotops measurments are
#' @param color.column conventionaly it is 'color', dot, and the name of the field
#' used to get the colors. By default 'object' ('color.object')
#' @param background.color color of the plotly background, 'black' or 'white',
#'  by default 'black'
#' @param title.plot the title of the plot
#' @param export.plot if TRUE, export the plot
#' @param out.plot the name of the output 3D plot
#' @param dirOut path of the output directory
#'
#' @return a plotly HML 3D plot
#'
#' @examples
#'
#' df.isotop <- isotop_dataframe()
#' isotop_3d(df.isotop)
#'
#'
#' @export
isotop_3d <- function(df = NA,
                      vars = c("var1", "var2", "var3"),
                      color.column = "color.object",
                      marker.size = 5,
                      background.color = "black",
                      title.plot = 'LIA measurements',
                      export.plot = FALSE,
                      out.plot = "isotop_3d.html",
                      dirOut = paste0(system.file(package = "itineRis"), "/results/")){
  type <- unlist(stringr::str_split(color.column, "\\."))[2]
  if(background.color == "black"){
    bg.color <- c('rgb(0, 0, 0)', 'rgb(255, 255, 255)', "white")
  } else {
    bg.color <- c('rgb(255, 255, 255)', 'rgb(0, 0, 0)', "black")
  }
  d3 <- plotly::plot_ly(df,
                        x = df[ , vars[1]],
                        y = df[ , vars[2]],
                        z = df[ , vars[3]],
                        size = marker.size,
                        color = df[ , type],
                        colors = df[ , color.column],
                        hoverinfo = 'text',
                        text = df[ , type])
  d3 <- d3 %>%  plotly::add_markers()
  d3 <- d3 %>%  plotly::layout(scene = list(xaxis = list(gridcolor = bg.color[2],
                                                         ticklen = 5,
                                                         gridwidth = 2),
                                            yaxis = list(gridcolor = bg.color[2],
                                                         ticklen = 5,
                                                         gridwith = 2),
                                            zaxis = list(gridcolor = bg.color[2],
                                                         ticklen = 5,
                                                         gridwith = 2)),
                               paper_bgcolor = bg.color[1],
                               plot_bgcolor = bg.color[1],
                               font = list(color = bg.color[3])
  )
  if (export.plot) {
    dir.create(dirOut, showWarnings = FALSE)
    htmlwidgets::saveWidget(d3, paste0(dirOut, out.plot))
    print(paste("interactive 3D plot '", out.plot, "' created in '", dirOut, "'"))
  } else {
    print(d3)
  }
}

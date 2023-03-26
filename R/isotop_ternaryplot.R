#' Create an interactive ternary plot from a dataset
#'
#' @name isotop_ternaryplot
#'
#' @description
#'
#' @param df a dataframe with LIA values.
#' @param export.plot if TRUE will export.
#' @verbose if TRUE, print messages.
#'
#' @return a Plotly interactive ternary plot
#'
#' @examples
#'
#' d <- hash::hash()
#'
#' d <- read_lia(d = d,
#'               d.tag = "lia.objects",
#'               df.path = "C:/Rprojects/itineRis/results/LIA data objects.xlsx",
#'               header.line = 2)
#' d <- read_lia(d = d,
#'               d.tag = "lia.mines",
#'               pattern.objects.num = "Locality/.Mine",
#'               pattern.objects.Pb206_Pb204 = "206Pb/204Pb",
#'               pattern.objects.Pb207_Pb204 = "207Pb/204Pb",
#'               pattern.objects.Pb208_Pb204 = "208Pb/204Pb",
#'               df.path = "C:/Rprojects/itineRis/results/Coordinates-mines_Thomas Huet.xlsx",
#'               header.line = 1)
#'
#' library(dplyr)
#'
#' mydf <- rbind(d$lia.mines, d$lia.objects)
#' df.isotop <- isotop_dataframe(df = mydf, df.path = NA)
#' df.isotop[df.isotop$object == "golasecca", "color.object"] <- "#0000FF"
#' df.isotop[df.isotop$object == "hochdorf", "color.object"] <- "#00FFFF"
#' df.isotop[df.isotop$object == "France", "color.object"] <- "#FF0000"
#' df.isotop[df.isotop$object == "France", "symbol"] <- "triangle"
#' df.isotop[df.isotop$object == "Iberian Peninsula", "color.object"] <- "#FFA500"
#' df.isotop[df.isotop$object == "Iberian Peninsula", "symbol"] <- "triangle"
#' df.isotop[df.isotop$object == "Switzerland", "color.object"] <- "#964B00"
#' df.isotop[df.isotop$object == "Switzerland", "symbol"] <- "triangle"
#'
#' ## Plot
#' isotop_ternaryplot(df.isotop)
#'
#' ## Export
#' isotop_ternaryplot(df.isotop, export.plot = T,
#'                    outDir = "C:/Rprojects/itineRis/results/", outFile = "lia_tern_2")
#'
#' @export
isotop_ternaryplot <- function(df = NA,
                               export.plot = FALSE,
                               outDir = paste0(system.file(package = 'itineRis'), "/results"),
                               outFile = "tern_lia",
                               verbose = FALSE){
  # TODO: pass this assignations to itineRis 'symbol' calculation
  # df <- df.isotop
  df$symbol <- NULL
  symbols.default <- c('circle', 'square', 'triangle', 'diamond', 'star', 'cross')
  objects.used <- as.character(unique(df$object))
  symbols.used <- symbols.default[c(1:length(objects.used))]
  symbols.objects <- data.frame(object = objects.used,
                                symbol = symbols.used)

  df <- merge(df, symbols.objects, by = "object", all.x = TRUE)
  df$Pb206_Pb204.perc <- (df$Pb206_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  df$Pb207_Pb204.perc <- (df$Pb207_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  df$Pb208_Pb204.perc <- (df$Pb208_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 100,
    pad = 20
  )
  df$lbl <- paste0(df$num, "\n",
                   "Pb206/204: ", round(df$Pb206_Pb204.perc, 2), "% \n",
                   "Pb207/204: ", round(df$Pb207_Pb204.perc, 2), "% \n",
                   "Pb208/204: ", round(df$Pb208_Pb204.perc, 2), "%")
  min206 <- min(df$Pb206_Pb204.perc)
  min207 <- min(df$Pb207_Pb204.perc)
  min208 <- min(df$Pb208_Pb204.perc)
  fig <- plotly::plot_ly(data = df,
                 name = ~object,
                 color = ~color.object) %>%
    plotly::add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~Pb206_Pb204.perc,
      b = ~Pb207_Pb204.perc,
      c = ~Pb208_Pb204.perc,
      marker = list(
        symbol = ~symbol,
        size = 10,
        opacity = .7,
        line = list('width' = 1,
                    color = '#00000070')
      )) %>%
    plotly::add_trace(
      type = 'scatterternary',
      mode = 'text',
      a = ~Pb206_Pb204.perc,
      b = ~Pb207_Pb204.perc,
      c = ~Pb208_Pb204.perc,
      hovertext = ~lbl,
      hoverinfo = 'text',
      marker = list(
        color = ~color.object,
        size = 10,
        opacity = 0,
        line = list('width' = 1,
                    color = '#00000070')),
      showlegend = F,
      inherit = F
    ) %>%
    plotly::layout(
      title = list(text = "Relative % of lead isotopes for mines and EIA items", x = 1),
      margin = m,
      ternary = list(
        legend = list(orientation = "h"),
        sum = 100,
        aaxis = list(min = min206, title = '<sup>206</sup>Pb/<sup>204</sup>Pb'),
        baxis = list(min = min207, title = '<sup>207</sup>Pb/<sup>204</sup>Pb'),
        caxis = list(min = min208, title = '<sup>208</sup>Pb/<sup>204</sup>Pb')
      )
    )
  if(export.plot){
    dir.create(outDir, showWarnings = FALSE)
    htmlwidgets::saveWidget(fig, paste0(outDir, "/", outFile, ".html"))
  } else {
    print(fig)
  }
}


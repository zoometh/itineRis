#' Create a ternary plot from a dataset
#' @name isotop_ternaryplot
#'
#' @description
#'
#' @param df a dataframe created with the isotop_dataframe() function.
#' @verbose if TRUE, print messages
#'
#' @return a dataframe with an hexadecimal value for the colors
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
#' isotop_ternaryplot(df.isotop)
#'
#' @export
isotop_ternaryplot <- function(df = NA,
                               verbose = FALSE){
  df$Pb206_Pb204.perc <- (df$Pb206_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  df$Pb207_Pb204.perc <- (df$Pb207_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  df$Pb208_Pb204.perc <- (df$Pb208_Pb204/(df$Pb206_Pb204 + df$Pb207_Pb204 + df$Pb208_Pb204))*100
  df$lbl <- paste0(df$num, "\n",
                          "Pb206/204: ", round(df$Pb206_Pb204.perc, 2), "% \n",
                          "Pb207/204: ", round(df$Pb207_Pb204.perc, 2), "% \n",
                          "Pb208/204: ", round(df$Pb208_Pb204.perc, 2), "%")
  min206 <- min(df$Pb206_Pb204.perc)
  min207 <- min(df$Pb207_Pb204.perc)
  min208 <- min(df$Pb208_Pb204.perc)
  fig <- plotly::plot_ly(df) %>%
    plotly::add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~Pb206_Pb204.perc,
      b = ~Pb207_Pb204.perc,
      c = ~Pb208_Pb204.perc,
      text = ~lbl,
      hoverinfo='text',
      symbol = ~symbol,
      marker = list(
        color = ~color.object,
        size = 10,
        line = list('width' = 1, color = 'black')
      )
    ) %>%
    plotly::layout(
      title = "Relative percentages of lead isotops",
      ternary = list(
        legend = list(title = list(text='<b>Lead Isotops</b>'),
                      orientation = "h"),
        sum = 100,
        aaxis = list(min = min206, title = 'Pb206/Pb204'),
        baxis = list(min = min207, title = 'Pb207/Pb204'),
        caxis = list(min = min208, title = 'Pb208/Pb204')
      )
    )
  print(fig)
}


#' Create an interactive ternary plot from a dataset
#'
#' @name isotop_ternaryplot
#'
#' @description Create an interactive ternary plot from a dataset, whether fully interactive (R Shiny + Plotly) or partly interactive (Plotly)
#'
#' @param df a dataframe with isotop values.
#' @param shiny if TRUE (Default), interactive (Shiny)
#' @param export.plot if TRUE will export.
#' @param verbose if TRUE, print messages.
#'
#' @return a Plotly interactive ternary plot
#'
#' @examples
#'
#' # Not interactive, with LIA isotops
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
#' ## Interactive
#' isotop_ternaryplot(run.shiny = T)
#'
#' ## Not interactive
#' isotop_ternaryplot(run.shiny = F)
#'
#' @export
#'
isotop_ternaryplot <- function(df = NA,
                               run.shiny = TRUE,
                               export.plot = FALSE,
                               outDir = paste0(system.file(package = 'itineRis'), "/results"),
                               outFile = "tern_lia",
                               verbose = TRUE){
  if(run.shiny){
    # Don't work
    # TODO: fix or suppress this shiny snippet
    if(verbose){print("Run the R Shiny app")}

    Sys.setlocale("LC_ALL", "C")

    library(shiny)
    library(plotly)
    library(dplyr)


    df.iso <- "C:/Users/Thomas Huet/Desktop/Shadreck data/XRF_Majors.csv"
    dfisotops <- read.csv2(df.iso, sep = ";", row.names = 1)
    # clean dataset
    dfisotops <- dfisotops %>%
      select_if(~ !any(is.na(.)))
    # intersect the colnames from the df and the thesaurus to find the correct columns if df
    thes <- isotop_thesaurus(as.list = T)
    studied.isotops <- intersect(colnames(dfisotops), thes)

    m <- list(
      l = 50,
      r = 50,
      b = 50,
      t = 100,
      pad = 20
    )

    shinyApp(
      ui <- fluidPage(
        br(), br(), br(), br(), br(), br(), br(), br(),
        tabPanel("Single", fluid = TRUE,
                 sidebarLayout(
                   position = "right",
                   sidebarPanel(
                     width = 2,
                     checkboxGroupInput("objects", "objects",
                                        studied.isotops,
                                        selected = studied.isotops[1:3]),

                     sliderInput("kmeans", "kmeans",
                                 min = 1, max = 5, value = 2)
                   ),
                   mainPanel(
                     plotlyOutput("graph",
                                  height = "800px")
                   )
                 )
        )
      ),
      server <- function(input, output, session){
        observe({
          if(length(input$objects) !=  3){
            updateCheckboxGroupInput(session, "objects", selected = tail(input$objects, 3))
          }
          # })
          output$graph <- renderPlotly({
            # here below replace studied.isotops[1], .., studied.isotops[3] with input$..
            # isotop1 <- studied.isotops[1]
            # isotop2 <- studied.isotops[2]
            # isotop3 <- studied.isotops[3]
            isotop1 <- input$objects[1]
            isotop2 <- input$objects[2]
            isotop3 <- input$objects[3]

            dfisotops.sel <- dfisotops[ , c(isotop1, isotop2, isotop3)]

            # statistics kmeans
            # kmeans with the user's nb of clusters
            nb.clust <- input$kmeans
            # nb.clust <- 3
            dfkmeans <- kmeans(x = dfisotops.sel[ , c(isotop1, isotop2, isotop3)],
                               centers = nb.clust,
                               nstart = 20)
            dfisotops.sel$cluster <- dfkmeans$cluster
            all.colors <- c("red", "blue", "green", "orange", "pink")
            colors <- all.colors[1 : nb.clust]
            # colors <- RColorBrewer::brewer.pal(input$kmeans, "Set1")
            dfcolors <- data.frame(cluster = seq(1, nb.clust),
                                   cluster.color = colors)
            dfisotops.sel$id <- rownames(dfisotops.sel)
            dfisotops.sel <- merge(dfisotops.sel, dfcolors, by = "cluster", all.x = T)
            row.names(dfisotops.sel) <- dfisotops.sel$id
            dfisotops.sel <- dfisotops.sel[order(row.names(dfisotops.sel)), ]

            # statistics %
            dfisotops.sel$tot <- rowSums(dfisotops.sel[ , c(isotop1, isotop2, isotop3)])
            isotop.val1.perc <- (dfisotops.sel[ , isotop1]/dfisotops.sel$tot)*100
            isotop.val2.perc <- (dfisotops.sel[ , isotop2]/dfisotops.sel$tot)*100
            isotop.val3.perc <- (dfisotops.sel[ , isotop3]/dfisotops.sel$tot)*100
            min1 <- min(isotop.val1.perc)
            min2 <- min(isotop.val2.perc)
            min3 <- min(isotop.val3.perc)

            # rownames(dfisotops.sel) <- rownames(dfisotops.sel.sel)
            dfisotops.sel$lbl <- paste0("<b>", rownames(dfisotops.sel), "</b>", "\n",
                                        isotop1, " %:", round(isotop.val1.perc, 2),
                                        " | abs: ", dfisotops.sel[ , isotop1], "\n",
                                        isotop2, " %:", round(isotop.val2.perc, 2),
                                        " | abs: ", dfisotops.sel[ , isotop2], "\n",
                                        isotop3, " %:", round(isotop.val3.perc, 2),
                                        " | abs: ", dfisotops.sel[ , isotop3])
            fig <- plot_ly(data = dfisotops.sel,
                           name = ~rownames(dfisotops.sel)) %>%
              add_trace(
                type = 'scatterternary',
                mode = 'markers',
                a = ~isotop.val1.perc,
                b = ~isotop.val2.perc,
                c = ~isotop.val3.perc,
                # text = ~lbl,
                # hoverinfo = 'text',
                #opacity = .3,
                marker = list(
                  # symbol = ~symbol,
                  #symbols = 'square', # unique(df.isotop$symbol),
                  color = ~cluster.color,
                  size = 12,
                  opacity = .6,
                  line = list('width' = 1,
                              color = '#00000070')
                )) %>%
              add_trace(
                type = 'scatterternary',
                mode = 'text',
                a = ~isotop.val1.perc,
                b = ~isotop.val2.perc,
                c = ~isotop.val3.perc,
                text = ~cluster,
                hovertext = ~lbl,
                # text = ~cluster,
                hoverinfo = 'text',
                #opacity = .3,
                marker = list(
                  # symbol = ~symbol,
                  # symbols = 'square',# unique(df.isotop$symbol),
                  #color = ~color.object,
                  size = 10,
                  opacity = 0,
                  line = list('width' = 1,
                              color = '#00000070')),
                showlegend = F,
                inherit = F
              ) %>%
              layout(
                # title = "Relative percentages of lead isotops",
                title = list(text = "Relative % of isotopes", x = 1),
                margin = m,
                # title = list(orientation = "h",   # show entries horizontally
                #              xanchor = "center",  # use center of legend as anchor
                #              x = 0.5,
                #              text = "Relative percentages of lead isotops"),
                ternary = list(
                  legend = list(orientation = "h"),
                  sum = 100,
                  aaxis = list(min = min1, title = isotop1),
                  baxis = list(min = min2, title = isotop2),
                  caxis = list(min = min3, title = isotop3)
                ),
                legend = list(x = 0.1, y = 0.9)
              )
            # fig
            fig$config <- list(doubleClick = "resize")
            fig
          })
        })
      }
    )
    # shinyApp(ui, server, options=c(shiny.launch.browser = .rs.invokeShinyPaneViewer))
  }
  if(!run.shiny){
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
}

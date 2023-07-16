
###

# FOR SHADRECK

# based on:
# Teaching app
# data with 3 dimensions

isotop_ternaryplot_shiny <- function() {

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
}

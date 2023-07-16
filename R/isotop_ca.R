#' Correspondance Analysis on isotop data
#'
#' @name isotop_ca
#'
#' @description Correspondance Analysis on isotop data. Inspired by the `zoo_ca()` function of zoowork
#'
#' @param df.iso a dataframe
#' @param iso.preselect a preselection of the isotops that will be analysed, in a vector form, ex: `c(1, 2, 3, 4, 5)`. If provided, will skip the user prompt. Default NA.
#' @param num_column the column name of assemblage numbers
#' @param pt_siz the size of the symbols
#' @param export.plot if TRUE (Default: FALSE), save the plot. If not, will plot it
#' @param dirOut the output directory
#' @param ca.name the name of the output plot if saved
#' @param plot.width,plot.height,plot.dpi the dimensions and resolution of the plot if saved
#' @param verbose if TRUE (default): verbose
#'
#' @return a gglot with the different CA by period
#'
#' @examples
#'
#' # Run on preselected isotops (1 to 5) only
#' isotop_ca(iso.preselect = c(1, 2, 3, 4, 5))
#'
#' # Export
#' isotop_ca(export.plot = T,
#'           dirOut = "C:/Users/Thomas Huet/Desktop/Shadreck data/")
#'
#' @export
isotop_ca <- function(df.iso = "C:/Users/Thomas Huet/Desktop/Shadreck data/XRF_Majors.csv",
                      iso.preselect = NA,
                      num_column = "num",
                      pt_siz = 1.5,
                      export.plot = FALSE,
                      dirOut = paste0(system.file(package = "itineRis"), "/results/"),
                      ca.name = "ca.png",
                      plot.width = 22.2,
                      plot.height = 19.5,
                      plot.dpi = 300,
                      verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  dfisotops <- read.csv2(df.iso, sep = ";", row.names = 1)
  dfisotops <- dfisotops %>%
    dplyr::select_if(~ !any(is.na(.)))
  # intersect the colnames from the df and the thesaurus to find the correct columns if df
  thes <- isotop_thesaurus(as.list = T)
  measured.isotops <- intersect(colnames(dfisotops), thes)
  if(is.na(iso.preselect)){
  # user's choice
  indexes <- 1:length(measured.isotops)
  isotop_list <- paste0(indexes, ": ", measured.isotops)
  msg.a <- paste0("a) Select 3 to ", length(measured.isotops)-1, " isotops separated by commas (ex: 1,3,5,6) to select individualy, or by colons (1:10) to select a range (DO NOT MIX THE TWO OPTIONS")
  msg.b <- paste0("b) type 'A' to select all:")
  message_isotop_list <- paste0(msg.a, msg.b)
  # get user selection
  selected.iso <- readline(prompt = cat(isotop_list, msg.a, msg.b, sep = "\n"))

  # parse user's selection
  if(grepl(",", selected.iso)){
    iso.selection <- as.vector(as.integer(unlist(strsplit(selected.iso, ","))))
  }
  if(grepl(":", selected.iso)){
    iso.selection <- as.integer(eval(parse(text = selected.iso)))
  }
  if(tolower(selected.iso) == 'a'){
    iso.selection <- seq(1, length(measured.isotops))
  }
  # selected isotops
  studied.isotops <- measured.isotops[iso.selection]
  } else {
    # use the preselection
    studied.isotops <- measured.isotops[iso.preselect]
  }
  # dataset
  xdat <- dfisotops[ , studied.isotops]
  # symbology
  var.symb <- data.frame(Row.names = studied.isotops,
                         shape = rep(17, length(studied.isotops)),
                         color = rep("black", length(studied.isotops))
  )
  ind.symb <- data.frame(Row.names = rownames(xdat),
                         shape = rep(16, nrow(xdat)),
                         color = rep("blue", nrow(xdat))
  )
  if(verbose){print("  - run CA")}
  # xdat <- df_lda.per[ , -which(names(df_lda.per) %in% c(typSite_column))]
  ca <- FactoMineR::CA(xdat, graph = FALSE)            # AFC
  inertCA1 <- round(as.numeric(ca$eig[, 2][1]), 1)
  inertCA2 <- round(as.numeric(ca$eig[, 2][2]), 1)
  df.ca.perc <- data.frame(
    perCA1 = inertCA1,
    perCA2 = inertCA2
  )
  coords_ind_ca <- as.data.frame(ca$row$coord)
  coords_var_ca <- as.data.frame(ca$col$coord)
  coords_ca <- rbind(coords_ind_ca, coords_var_ca)
  colnames(coords_ca)[1] <- 'CA1'
  colnames(coords_ca)[2] <- 'CA2'
  dataset.p <- merge(xdat, coords_ca, by = "row.names", all.y = T)
  # symbols
  dfsymb <- rbind(ind.symb, var.symb)
  dataset.ps <- merge(dataset.p, dfsymb, by = "Row.names", all.x = T)
  dataset.ps$shape <- as.factor(dataset.ps$shape)
  names(dataset.ps)[names(dataset.ps) == 'Row.names'] <- num_column
  # ff <- merge(dataset.ps, df_per_site, by = num_column, all.x = T)
  # matches <- colnames(dataset.ps) # reorder
  # ff <- ff[ ,match(matches, colnames(ff))]
  # dataset.ps <- rbind(dataset.ps,ff)
  # perCA_tsit <- perCA_tsit[-1, ] #remove line 1 = 'xxx'
  # dataset.ps<- dataset.ps[-1, ]
  # dataset.ps$shape <- as.factor(dataset.ps$shape)
  # dataset.ps$color <- as.factor(dataset.ps$color)
  # CA
  # dataset.ps <- dataset.ps
  gca <- ggplot2::ggplot(dataset.ps, ggplot2::aes(CA1, CA2)) +
    # ggplot2::geom_text(ggplot2::aes(#x = min(CA1_interval),
    #                                 #y = max(CA2_interval),
    #                                 label = "my label"),
    #                    hjust = 0,
    #                    vjust = 1) +
    ggplot2::geom_point(ggplot2::aes(CA1, CA2,
                                     colour = color,
                                     fill = color,
                                     stroke = .5,
                                     pch = shape),
                        size = pt_siz) +
    ggrepel::geom_text_repel(ggplot2::aes(CA1, CA2, label = num),
                             cex = 2,
                             segment.size = 0.1,
                             segment.alpha = 0.5,
                             max.overlaps = Inf) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        size = 0.2, alpha = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        size = 0.2, alpha = 0.3) +
    ggplot2::geom_text(data = df.ca.perc,
                       mapping = ggplot2::aes(x = 0, y = -Inf,
                                              label = paste0(perCA1,"%")),
                       vjust = -1,
                       size = 2,
                       alpha = 0.5
    ) +
    ggplot2::geom_text(data = df.ca.perc,
                       mapping = ggplot2::aes(x = -Inf, y = 0,
                                              label = paste0(perCA2, "%")),
                       vjust = 1,
                       angle = 90,
                       size = 2,
                       alpha = 0.5) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 5),
                   axis.title.x = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(size = 8)) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.2)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8),
                   strip.text.y = ggplot2::element_blank()) +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = 'black',
                                                        fill = NA,
                                                        size = 0.2)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'transparent')) +
    ggplot2::theme(panel.spacing.y = ggplot2::unit(0, "lines")) +
    # ggplot2::scale_x_continuous(limits = CA1_interval, expand = c(0, 0)) +
    # ggplot2::scale_y_continuous(limits = CA2_interval, expand = c(0, 0)) +
    ggplot2::scale_colour_identity() +
    # ggplot2::scale_shape_identity() +
    ggplot2::scale_fill_identity()
  if(export.plot){
    dir.create(dirOut, showWarnings = FALSE)
    gout <- paste0(dirOut, ca.name)
    ggplot2::ggsave(file = gout,
                    plot = gca,
                    width = plot.width,
                    height = plot.height,
                    units = "cm",
                    dpi = plot.dpi)  ## save plot
    if(verbose){print(paste0("The plot '", ca.name,"' has been saved in '", dirOut,"'"))}
  } else {
    return(gca)
  }
}


